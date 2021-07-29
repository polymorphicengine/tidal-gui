{-# LANGUAGE FlexibleInstances #-}

import System.FilePath  (dropFileName)
import System.Environment (getExecutablePath)

import Control.Concurrent (forkIO)
import Control.Concurrent.MVar  (newEmptyMVar, tryTakeMVar, MVar, putMVar, newMVar, takeMVar)
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
     high <- liftIO newEmptyMVar
     pats <- liftIO $ newMVar Map.empty
     void $ liftIO $ forkIO $ highlightLoop [] str win high

     let env = Env win str output high pats
         evaluateBlock = runReaderT (interpretCommands False) env
         evaluateLine = runReaderT (interpretCommands True) env

     createHaskellFunction "evaluateBlock" evaluateBlock
     createHaskellFunction "evaluateLine" evaluateLine
     createHaskellFunction "hush" (hush str pats high)
     createHaskellFunction "muteH1" (muteH str high "h1")
     createHaskellFunction "muteH2" (muteH str high "h2")
     createHaskellFunction "muteH3" (muteH str high "h3")
     createHaskellFunction "muteH4" (muteH str high "h4")
     createHaskellFunction "muteH5" (muteH str high "h5")
     createHaskellFunction "muteH6" (muteH str high "h6")
     createHaskellFunction "muteH7" (muteH str high "h7")
     createHaskellFunction "muteH8" (muteH str high "h8")
     createHaskellFunction "muteH9" (muteH str high "h9")

     createHaskellFunction "muteP1" (muteP str pats 1)
     createHaskellFunction "muteP2" (muteP str pats 2)
     createHaskellFunction "muteP3" (muteP str pats 3)
     createHaskellFunction "muteP4" (muteP str pats 4)
     createHaskellFunction "muteP5" (muteP str pats 5)
     createHaskellFunction "muteP6" (muteP str pats 6)
     createHaskellFunction "muteP7" (muteP str pats 7)
     createHaskellFunction "muteP8" (muteP str pats 8)
     createHaskellFunction "muteP9" (muteP str pats 9)
     -- put elements on body
     UI.getBody win #+ [element definitions, element ctrl, element load, element save, element settings, element makeCtrlEditor, element makeDefsEditor, element output]

data Env = Env {windowE :: Window
                ,streamE :: Stream
                ,outputE :: Element
                ,patH :: MVar HighlightStates
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
           highStatesMVar = patH env
           patStatesMVar = patS env
           p = streamReplace str
       contentsControl <- liftUI editorValueControl
       contentsDef <- liftUI editorValueDefinitions
       line <- liftUI getCursorLine
       let bs = getBlocks contentsControl
           blockMaybe = if lineBool then getLineContent line (linesNum contentsControl) else getBlock line bs
       case blockMaybe of
           Nothing -> void $ liftUI $ element out # set UI.text "Failed to get Block"
           Just (Block blockLineStart blockLineEnd block) -> do
                   case parse parseCommand "" block of
                         Left e -> errorUI $ show e
                         Right command -> case command of
                                         (H name s (ln,ch)) -> do
                                                 res <- liftIO $ runHintPattern True s contentsDef
                                                 case res of
                                                     Right (Right pat) -> do
                                                                       successUI >> (outputUI "")
                                                                       liftIO $ p name $ pat
                                                                       highStates <- liftIO $ tryTakeMVar highStatesMVar
                                                                       case highStates of
                                                                             Just pats -> do
                                                                                 let newPatS = Map.insert name (HS pat (blockLineStart + ln) ch False False) pats
                                                                                 liftIO $ putMVar highStatesMVar $ newPatS
                                                                             Nothing -> do
                                                                                 let newPatS = Map.insert name (HS pat (blockLineStart + ln) ch False False) Map.empty
                                                                                 liftIO $ putMVar highStatesMVar $ newPatS
                                                     Right (Left e) -> errorUI $ parseError e
                                                     Left e -> errorUI $ show e
                                         (Hush)      -> successUI >> (liftIO $ hush str patStatesMVar highStatesMVar)
                                         (Cps x)     -> successUI >> (liftIO $ streamOnce str $ cps (pure x))
                                         (Other s)   -> do
                                                 res <- liftIO $ runHintStatement True s contentsDef str
                                                 case res of
                                                   Right "()" -> successUI
                                                   Right (['\"','d',num,'\"']) -> do
                                                                    successUI >> (outputUI "")
                                                                    patStates <- liftIO $ takeMVar patStatesMVar
                                                                    let newPatS = Map.insert (read [num]) (PS (read [num]) False False) patStates
                                                                    liftIO $ putMVar patStatesMVar $ newPatS
                                                   Right outputString -> successUI >> (outputUI outputString)
                                                   Left e -> errorUI $ parseError e
                                         (T s)        -> do
                                                  res <- liftIO $ getType True s contentsDef str
                                                  case res of
                                                    (Right t) -> successUI >> (outputUI t)
                                                    (Left e) -> errorUI $ parseError e
            where successUI = liftUI $ flashSuccess blockLineStart blockLineEnd
                  errorUI string = (liftUI $ flashError blockLineStart blockLineEnd) >> (void $ liftUI $ element out # C.set UI.text string)
                  outputUI string = void $ liftUI $ element out # set UI.text string
