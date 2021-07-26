{-# LANGUAGE FlexibleInstances #-}

import System.FilePath  (dropFileName)
import System.Environment (getExecutablePath)

import Control.Concurrent (forkIO)
import Control.Concurrent.MVar  (newEmptyMVar, tryTakeMVar, MVar, putMVar)
import Control.Monad  (void)
import Control.Monad.Reader (ReaderT, runReaderT, ask)

import Data.Map as Map (insert, empty)

import Sound.Tidal.Context as T hiding (mute,solo,(#),s)

import Text.Parsec  (parse)

import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core as C hiding (text)

import Parse
import Highlight
import Ui
import Configure
import Hint

main :: IO ()
main = do
    execPath <- dropFileName <$> getExecutablePath
    str <- T.startStream T.defaultConfig [(T.superdirtTarget {T.oLatency = 0.1},
                                              [T.superdirtShape]
                                             ),
                                             (remoteTarget,
                                              [T.OSCContext "/code/highlight"]
                                             )
                                            ]
    startGUI C.defaultConfig {
          jsStatic = Just $ execPath ++ "static",
          jsCustomHTML     = Just "tidal.html"
        } $ setup str


setup :: Stream -> Window -> UI ()
setup str win = void $ do
     --setup GUI
     void $ return win # set title "Tidal"
     definitions <- UI.textarea
                 # set (attr "id") "definitions-editor"
     ctrl <- UI.textarea
                 # set (attr "id") "control-editor"

     output <- UI.pre #+ [ string "output goes here" ]
     load <- UI.input
                  # set (attr "type") "file"
                  # set (attr "id") "fileInput"
                  # set (attr "onchange") "controlLoadFile()"
     save <- UI.button
                  # set UI.text "Save file"
                  # set (attr "onclick") "controlSaveFile()"

     execPath <- liftIO $ dropFileName <$> getExecutablePath
     tidalKeys <- liftIO $ readFile $ execPath ++ "static/tidalConfig.js"
     settings <- mkElement "script" # set UI.text tidalKeys
     makeCtrlEditor <- mkElement "script"
                       # set UI.text "const controlEditor = CodeMirror.fromTextArea(document.getElementById('control-editor'), controlEditorSettings);"
     makeDefsEditor <- mkElement "script"
                       # set UI.text "const definitionsEditor = CodeMirror.fromTextArea(document.getElementById('definitions-editor'), definitionsEditorSettings);"

     --highlight (experimental)
     pats <- liftIO $ newEmptyMVar
     void $ liftIO $ forkIO $ highlightLoop [] str win pats

     let env = Env win str output pats
         evaluateBlock = runReaderT (interpretCommands False) env
         evaluateLine = runReaderT (interpretCommands True) env

     createHaskellFunction "evaluateBlock" evaluateBlock
     createHaskellFunction "evaluateLine" evaluateLine
     createHaskellFunction "hush" (bigHush str pats)
     createHaskellFunction "mute1" (mute str pats 1)
     createHaskellFunction "mute2" (mute str pats 2)
     createHaskellFunction "mute3" (mute str pats 3)
     createHaskellFunction "mute4" (mute str pats 4)
     createHaskellFunction "mute5" (mute str pats 5)
     createHaskellFunction "mute6" (mute str pats 6)
     createHaskellFunction "mute7" (mute str pats 7)
     createHaskellFunction "mute8" (mute str pats 8)
     createHaskellFunction "mute9" (mute str pats 9)

     -- put elements on body
     UI.getBody win #+ [element definitions, element ctrl, element load, element save, element settings, element makeCtrlEditor, element makeDefsEditor, element output]

data Env = Env {windowE :: Window
                ,streamE :: Stream
                ,outputE :: Element
                ,patS :: MVar PatternStates
                }

-- to combine UI and IO actions with an environment
instance MonadUI (ReaderT Env IO) where
 liftUI m = do
           env <- ask
           let win = windowE env
           liftIO $ runUI win m

interpretCommands :: Bool -> ReaderT Env IO ()
interpretCommands lineBool = do
       env <- ask
       let out = outputE env
           str = streamE env
       contentsControl <- liftUI editorValueControl
       contentsDef <- liftUI editorValueDefinitions
       line <- liftUI getCursorLine
       let bs = getBlocks contentsControl
           blockMaybe = if lineBool then getLineContent line (linesNum contentsControl) else getBlock line bs
       case blockMaybe of
           Nothing -> void $ liftUI $ element out # set UI.text "Failed to get Block"
           Just (Block blockLineStart blockLineEnd block) -> do
                   let parsed = parse parseCommand "" block
                       p = streamReplace str
                   case parsed of
                         Left e -> do
                           liftUI $ flashError blockLineStart blockLineEnd
                           void $ liftUI $ element out # set UI.text ( "Parse error in " ++ show e )
                         Right command -> case command of
                                         (H num s (ln,ch)) -> do
                                                 res <- liftIO $ runHintSafe s contentsDef
                                                 case res of
                                                     Right (Right pat) -> do
                                                                       liftUI $ flashSuccess blockLineStart blockLineEnd
                                                                       let patStatesMVar = patS env
                                                                       void $ liftUI $ element out # set UI.text (show pat )
                                                                       liftIO $ p num $ pat |< orbit (pure $ num-1)
                                                                       patStates <- liftIO $ tryTakeMVar patStatesMVar
                                                                       case patStates of
                                                                             Just pats -> do
                                                                                 let newPatS = Map.insert num (PS pat (blockLineStart + ln) ch False False) pats
                                                                                 liftIO $ putMVar patStatesMVar $ newPatS
                                                                             Nothing -> do
                                                                                 let newPatS = Map.insert num (PS pat (blockLineStart + ln) ch False False) Map.empty
                                                                                 liftIO $ putMVar patStatesMVar $ newPatS
                                                     Right (Left e) -> do
                                                                     liftUI $ flashError blockLineStart blockLineEnd
                                                                     void $ liftUI $ element out # set UI.text (parseError e)
                                                     Left e -> do
                                                                     liftUI $ flashError blockLineStart blockLineEnd
                                                                     void $ liftUI $ element out # set UI.text (show e)
                                         (Hush)      -> do
                                                 liftUI $ flashSuccess blockLineStart blockLineEnd
                                                 liftIO $ bigHush str (patS env)
                                         (Cps x)     -> do
                                                 liftUI $ flashSuccess blockLineStart blockLineEnd
                                                 liftIO $ streamOnce str $ cps (pure x)
                                         (Other s)   -> do
                                                 res <- liftIO $ runHintSafeOther s contentsDef str
                                                 case res of
                                                   Right (Right action) -> do
                                                                   liftUI $ flashSuccess blockLineStart blockLineEnd
                                                                   liftIO $ action
                                                   Right (Left e) -> do
                                                                   liftUI $ flashError blockLineStart blockLineEnd
                                                                   void $ liftUI $ element out # C.set UI.text (parseError e)
                                                   Left e -> do
                                                                   liftUI $ flashError blockLineStart blockLineEnd
                                                                   void $ liftUI $ element out # C.set UI.text (show e)
                                         (T s)        -> do
                                                  res <- liftIO $ getTypeSafe s contentsDef str
                                                  case res of
                                                    (Right t) -> do
                                                                  liftUI $ flashSuccess blockLineStart blockLineEnd
                                                                  void $ liftUI $ element out # set UI.text t
                                                    (Left e) -> do
                                                                  liftUI $ flashError blockLineStart blockLineEnd
                                                                  void $ liftUI $ element out # C.set UI.text (parseError e)
