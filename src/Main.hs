{-# LANGUAGE FlexibleInstances #-}

import       Language.Haskell.Interpreter as Hint
import       Language.Haskell.Interpreter.Unsafe as Hint
import       Sound.Tidal.Context as T
import       Control.Concurrent.MVar
import       Control.Concurrent
import Control.Monad (void)
import Control.Monad.Reader
import Sound.Tidal.Stream (Target(..))
import Text.Parsec(parse)

import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core as C hiding (text)
-- import Graphics.UI.Threepenny.Internal(unUI)


import Parse

data Env = Env {window :: Window
               ,stream :: Stream
               ,input :: Element
               ,output :: Element
               ,errors :: Element
               ,strKey :: MVar Bool
               ,enterKey :: MVar Bool
               }

libs = [
    "Sound.Tidal.Context"
  , "Sound.Tidal.Simple"
  , "Control.Applicative"
  , "Data.Bifunctor"
  , "Data.Bits"
  , "Data.Bool"
  , "Data.Char"
  , "Data.Either"
  , "Data.Foldable"
  , "Data.Function"
  , "Data.Functor"
  , "Data.Int"
  , "Data.List"
  , "Data.Map"
  , "Data.Maybe"
  , "Data.Monoid"
  , "Data.Ord"
  , "Data.Ratio"
  , "Data.Semigroup"
  , "Data.String"
  , "Data.Traversable"
  , "Data.Tuple"
  , "Data.Typeable"
  , "GHC.Float"
  , "GHC.Real"
  ]

exts = [OverloadedStrings, NoImplicitPrelude]

listenPort = 6011
remotePort = 6012

main :: IO ()
main = do
        putStrLn "Please enter path to static:"
        static <- readLn
        let remoteTarget = Target {oName = "atom",
                                   oAddress = "127.0.0.1",
                                   oPort = remotePort,
                                   oBusPort = Nothing,
                                   oLatency = 0.1,
                                   oWindow = Nothing,
                                   oSchedule = T.Live,
                                   oHandshake = True}
        stream <- T.startStream T.defaultConfig [(T.superdirtTarget {oLatency = 0.1},
                                                  [T.superdirtShape]
                                                 ),
                                                 (remoteTarget,
                                                  [T.OSCContext "/code/highlight"]
                                                 )
                                                ]
        startGUI C.defaultConfig {
              jsStatic = Just static,
              jsCustomHTML     = Just "tidal.html"
            } $ setup stream

--this is a bit hacky, when calling get, el is actually not needed
editorValue :: Attr Element String
editorValue = mkReadWriteAttr get set
    where
      get el = callFunction $ ffi "codemirrorEditor.getValue()"
      set v el = runFunction  $ ffi "$(%1).val(%2)" el v

setup :: Stream -> Window -> UI ()
setup stream win = void $ do

      return win C.# C.set title "Tidal"

      input <- UI.textarea
                  C.# C.set (attr "id") "code"

      output <- UI.div #+ [ string "output goes here" ]
      errors <- UI.div #+ [ string "errors go here" ]

      strKey <- liftIO $ newMVar False
      enterKey <- liftIO $ newMVar False

      let env = Env win stream input output errors strKey enterKey

      body <- UI.getBody win
      on UI.keydown body $ \x -> liftIO $ runReaderT (keydownEvent x) env
      on UI.keyup body $ \x -> liftIO $ runReaderT (keyupEvent x) env

      script <- mkElement "script"
                  C.# C.set UI.text "const codemirrorEditor = CodeMirror.fromTextArea(document.getElementById('code'), {lineNumbers: true, mode: \"haskell\"});"

      UI.getBody win #+ [element input, element script, element errors, element output]

instance MonadUI (ReaderT Env IO) where
  liftUI m = do
            env <- ask
            let win = window env
            liftIO $ runUI win m

keydownEvent :: Int -> ReaderT Env IO ()
keydownEvent x = do
            env <- ask
            let str = strKey env
                enter = enterKey env
            case x of
                17 -> do
                  liftIO $ takeMVar str
                  liftIO $ putMVar str True
                  enter <- liftIO $ readMVar enter
                  case enter of
                    True -> do
                          liftIO $ putStrLn "hey"
                          interpretC
                    _ -> return ()
                13 -> do
                  liftIO $ takeMVar enter
                  liftIO $ putMVar enter True
                  str <- liftIO $ readMVar str
                  case str of
                    True -> interpretC
                    _ -> return ()
                _ -> return ()

keyupEvent :: Int -> ReaderT Env IO ()
keyupEvent x = do
              env <- ask
              let str = strKey env
                  enter = enterKey env
              case x of
                  17 -> do
                    liftIO $ takeMVar str
                    liftIO $ putMVar str False
                  13 -> do
                    liftIO $ takeMVar enter
                    liftIO $ putMVar enter False
                  _ -> return ()

interpretC :: ReaderT Env IO ()
interpretC  = do
        env <- ask
        let inp = input env
            out = output env
            err = errors env
            str = stream env
        contents <- liftUI $ C.get editorValue inp
        let parsed = parse parseCommand "" contents
            p = streamReplace str
        case parsed of
          Left e -> do
                liftUI $ element err C.# C.set UI.text ( "Parse Error:" ++ show e )
                return ()
          Right command -> case command of
                                  (D num string) -> do
                                          res <- liftIO $ Hint.runInterpreter $ do
                                              Hint.set [languageExtensions := exts]
                                              Hint.setImports libs
                                              Hint.interpret string (Hint.as :: ControlPattern)
                                          case res of
                                              Right pat -> do
                                                liftUI $ element out C.# C.set UI.text ( "control pattern:" ++ show pat )
                                                liftIO $ p num $ pat |< orbit (pure $ num-1)
                                              Left  e -> do
                                                liftUI $ element err C.# C.set UI.text ( "Interpreter Error:" ++ show e )
                                                return ()
                                  (Hush)      -> liftIO $ streamHush str
                                  (Cps x)   -> liftIO $ streamOnce str $ cps (pure x)
