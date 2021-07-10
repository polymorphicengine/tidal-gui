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
-- import Foreign.JavaScript.Marshal
-- import Graphics.UI.Threepenny.Internal(unUI)

import Parse

data Env = Env {window :: Window
               ,stream :: Stream
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

remoteTarget = Target {oName = "atom"
                      ,oAddress = "127.0.0.1"
                      ,oPort = remotePort
                      ,oBusPort = Nothing
                      ,oLatency = 0.1
                      ,oWindow = Nothing
                      ,oSchedule = T.Live
                      ,oHandshake = True
                      }

main :: IO ()
main = do
        putStrLn "Please enter path to static:"
        static <- readLn
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

--get the contents of the codeMirror editor
editorValueDefinitions :: UI String
editorValueDefinitions = callFunction $ ffi "definitionsEditor.getValue()"

editorValueControl :: UI String
editorValueControl = callFunction $ ffi "controlEditor.getValue()"

-- instance (FromJS a, FromJS b) => FromJS (a,b) where
--   fromJs (x,y) = (fromJS x, fromJS y)

getCursorLine :: UI Int
getCursorLine = callFunction $ ffi "(controlEditor.getCursor()).line"

getCursorCol :: UI Int
getCursorCol = callFunction $ ffi "(controlEditor.getCursor()).ch"

setup :: Stream -> Window -> UI ()
setup stream win = void $ do
      --setup GUI
      return win C.# C.set title "Tidal"
      definitions <- UI.textarea
                  C.# C.set (attr "id") "definitions-editor"
      control <- UI.textarea
                  C.# C.set (attr "id") "control-editor"

      output <- UI.div #+ [ string "output goes here" ]
      errors <- UI.div #+ [ string "errors go here" ]
      body <- UI.getBody win
      script1 <- mkElement "script"
                        C.# C.set UI.text "const controlEditor = CodeMirror.fromTextArea(document.getElementById('control-editor'), {lineNumbers: true, mode: \"haskell\"});"
      script2 <- mkElement "script"
                        C.# C.set UI.text "const definitionsEditor = CodeMirror.fromTextArea(document.getElementById('definitions-editor'), {lineNumbers: true, mode: \"haskell\"});"
      --setup env
      strKey <- liftIO $ newMVar False
      enterKey <- liftIO $ newMVar False

      let env = Env win stream output errors strKey enterKey

      --handle Events
      on UI.keydown body $ \x -> liftIO $ runReaderT (keydownEvent x) env
      on UI.keyup body $ \x -> liftIO $ runReaderT (keyupEvent x) env

      -- put elements on bod
      UI.getBody win #+ [element definitions, element control, element script1, element script2 , element errors, element output]

-- to combine UI and IO actions with an environment
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
        let out = output env
            err = errors env
            str = stream env
        contentsControl <- liftUI $ editorValueControl
        contentsDef <- liftUI $ editorValueDefinitions
        line <- liftUI getCursorLine
        let blocks = getBlocks contentsControl
            blockMaybe = getBlock line blocks
        case blockMaybe of
            Nothing -> do
                    liftUI $ element err C.# C.set UI.text "Failed to get Block"
                    return ()
            Just block -> do
                    let parsed = parse parseCommand "" block
                        p = streamReplace str
                    case parsed of
                          Left e -> do
                                liftUI $ element err C.# C.set UI.text ( "Parse Error:" ++ show e )
                                return ()
                          Right command -> case command of
                                                  (D num string) -> do
                                                          res <- liftIO $ runHintSafe string contentsDef
                                                          case res of
                                                              Right pat -> do
                                                                liftUI $ element out C.# C.set UI.text ( "control pattern:" ++ show pat )
                                                                liftUI $ element err C.# C.set UI.text "" 
                                                                liftIO $ p num $ pat |< orbit (pure $ num-1)
                                                              Left  e -> do
                                                                liftUI $ element err C.# C.set UI.text ( "Interpreter Error:" ++ show e )
                                                                return ()
                                                  (Hush)      -> liftIO $ streamHush str
                                                  (Cps x)   -> liftIO $ streamOnce str $ cps (pure x)

runHintSafe :: String -> String -> IO (Either InterpreterError ControlPattern)
runHintSafe input stmts = Hint.runInterpreter $ do
                          Hint.set [languageExtensions := exts]
                          Hint.setImports libs
                          Hint.runStmt stmts
                          Hint.interpret input (Hint.as :: ControlPattern)
