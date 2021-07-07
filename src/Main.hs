import       Language.Haskell.Interpreter as Hint
import       Language.Haskell.Interpreter.Unsafe as Hint
import       Sound.Tidal.Context
import       Control.Concurrent.MVar
import       Control.Concurrent
import Control.Monad (void)
import Sound.Tidal.Stream (Target(..))
import qualified Sound.Tidal.Context as T

import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core as C hiding (text)


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
        let p = streamReplace stream 1
        startGUI C.defaultConfig {
              jsStatic = Just static,
              jsCustomHTML     = Just "tidal.html"
            } $ setup p

--this is a bit hacky, when calling get, el is actually not needed
editorValue :: Attr Element String
editorValue = mkReadWriteAttr get set
    where
      get el = callFunction $ ffi "codemirrorEditor.getValue()"
      set v el = runFunction  $ ffi "$(%1).val(%2)" el v

setup :: (ControlPattern -> IO ()) -> Window -> UI ()
setup p win = void $ do

      return win C.# C.set title "Tidal"

      input <- UI.textarea
                  C.# C.set (attr "id") "code"

      output <- UI.div #+ [ string "output goes here" ]
      errors <- UI.div #+ [ string "errors go here" ]


      strKey <- liftIO $ newMVar False
      enterKey <- liftIO $ newMVar False

      body <- UI.getBody win
      on UI.keydown body $ \x -> keydownEvent strKey enterKey x input errors output
      on UI.keyup body $ \x -> liftIO $ keyupEvent strKey enterKey x

      script <- mkElement "script"
                  C.# C.set UI.text "const codemirrorEditor = CodeMirror.fromTextArea(document.getElementById('code'), {lineNumbers: true, mode: \"haskell\"});"

      UI.getBody win #+ [element input, element script, element errors, element output]
      where keydownEvent strKey enterKey x input errors output = do
                                          case x of
                                              17 -> do
                                                liftIO $ takeMVar strKey
                                                liftIO $ putMVar strKey True
                                                enter <- liftIO $ readMVar enterKey
                                                case enter of
                                                  True ->  interpretC p input errors output
                                                  _ -> return ()
                                              13 -> do
                                                liftIO $ takeMVar enterKey
                                                liftIO $ putMVar enterKey True
                                                str <- liftIO $ readMVar strKey
                                                case str of
                                                  True -> interpretC p input errors output
                                                  _ -> return ()
                                              _ -> return ()

keyupEvent :: MVar Bool -> MVar Bool -> Int -> IO ()
keyupEvent strKey enterKey x = do
              case x of
                  17 -> do
                    takeMVar strKey
                    putMVar strKey False
                  13 -> do
                    takeMVar enterKey
                    putMVar enterKey False
                  _ -> return ()

interpretC :: (ControlPattern -> IO ()) -> Element -> Element -> Element -> UI ()
interpretC p input errors output = do
                contents <- C.get editorValue input
                res <- liftIO $ Hint.runInterpreter $ do
                    Hint.set [languageExtensions := exts]
                    Hint.setImports libs
                    Hint.interpret contents (Hint.as :: ControlPattern)
                case res of
                    Right pat -> do
                      element output C.# C.set UI.text ( "control pattern:" ++ show pat )
                      liftIO $ p pat
                    Left  err -> do
                      element errors C.# C.set UI.text ( "error:" ++ show err )
                      return ()
