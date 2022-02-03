{-# LANGUAGE FlexibleInstances, OverloadedStrings #-}

import System.FilePath  (dropFileName)
import System.Environment (getExecutablePath)
import System.Directory (getDirectoryContents)

import Control.Concurrent (forkIO)
import Control.Concurrent.MVar  (newEmptyMVar, tryTakeMVar, MVar, putMVar, newMVar, takeMVar)
import Control.Monad  (void)
import Control.Monad.Reader (ReaderT, runReaderT, ask)

import Data.Map as Map (insert, empty)


import Sound.Tidal.Context as T hiding (mute,solo,(#),s)
import Sound.Tidal.ID

import Text.Parsec  (parse)

import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core as C hiding (text)

import System.IO.Silently

import Parse
import Highlight
import Ui
import Hint
import Visual


main :: IO ()
main = do
    execPath <- dropFileName <$> getExecutablePath
    (outTidal,str) <- capture $  T.startTidal (T.superdirtTarget {oLatency = 0.1, oAddress = "127.0.0.1", oPort = 57120}) (T.defaultConfig {cVerbose = True, cFrameTimespan = 1/20})

    startGUI C.defaultConfig {
          jsStatic = Just $ execPath ++ "static",
          jsCustomHTML     = Just "tidal.html"
        } $ setup str outTidal



data Env = Env {windowE :: Window
               ,streamE :: Stream
               ,outputE :: Element
               ,patH :: MVar HighlightStates
               ,patS :: MVar PatternStates
               ,hintM :: MVar InterpreterMessage
               ,hintR :: MVar InterpreterResponse
               ,eDefs :: MVar [String]
               }


setup :: Stream -> String -> Window -> UI ()
setup str stdout win = void $ do
     --setup GUI
     void $ return win # set title "TidalCycles"

     UI.addStyleSheet win "tidal.css"
     UI.addStyleSheet win "theme.css"

     setCallBufferMode NoBuffering -- important for highlighting

     ctrl <- UI.textarea # set (attr "id") "control-editor"

     output <- UI.pre #. "outputBox"
                      #+ [ string "output goes here" ]
                      # set style [("font-size","3vh")]
     display <- UI.pre #. "displayBox"
                       # set style [("font-size","3vh")]

     svg <- UI.div #. "svg-display"

     fileInput <- UI.input # set UI.id_ "fileInput"
                           # set UI.type_ "file"
                           # set style [("display","none")]

     inputScript <- mkElement "script" # set UI.text  "document.getElementById(\"fileInput\").onchange = e => { var file = e.target.files[0]; var reader = new FileReader();reader.readAsText(file,'UTF-8');reader.onload = function() {controlEditor.getDoc().setValue(reader.result);};}"

     winWidth <- getWindowWidth
     winHeight <- getWindowHeight

     canv <- UI.canvas #. "canvas"
                       # set UI.width (round $ winWidth*2)
                       # set UI.height (round $ winHeight*2)


     _ <- set UI.lineWidth 0.5 (return canv)

     execPath <- liftIO $ dropFileName <$> getExecutablePath
     tidalKeys <- liftIO $ readFile $ execPath ++ "static/tidalConfig.js"
     ghcMode <- liftIO $ readFile $ execPath ++ "static/ghc_mode.txt"
     recorderScript <- liftIO $ readFile $ execPath ++ "static/codemirror/cm-record.js"


     userDefsPaths <- liftIO $ getDirectoryContents $ execPath ++ "static/definitions/"
     bootDefs <- liftIO $ sequence $ map (\x -> readFile $ execPath ++ "static/definitions/" ++ x) $ filter (\s -> s /= "." && s /= "..") userDefsPaths

     settings <- mkElement "script" # set UI.text tidalKeys
     recorder <- mkElement "script" # set UI.text recorderScript

     makeCtrlEditor <- mkElement "script"
                       # set UI.text "const controlEditor = CodeMirror.fromTextArea(document.getElementById('control-editor'), controlEditorSettings);"

     --highlight (experimental)
     high <- liftIO newEmptyMVar
     pats <- liftIO $ newMVar Map.empty
     mMV <- liftIO newEmptyMVar
     rMV <- liftIO newEmptyMVar
     defsMV <- liftIO $ newMVar []
     colMV <- liftIO $ newMVar Map.empty -- could be initialised with custom colorMap, example: (Map.insert "bd" "black" Map.empty)

     void $ liftIO $ forkIO $ highlightLoop [] str win high

     createHaskellFunction "svgLoop" (visualizeStreamLoop win svg str colMV)
     void $ liftIO $ forkIO $ runUI win $ runFunction $ ffi "svgLoop()"

     createHaskellFunction "displayLoop" (displayLoop win display str)
     void $ liftIO $ forkIO $ runUI win $ runFunction $ ffi "requestAnimationFrame(displayLoop)"

     -- createHaskellFunction "canvasLoop" (visualizeStreamLoopCanv win canv str colMV)
     -- void $ liftIO $ forkIO $ runUI win $ runFunction $ ffi "canvasLoop()"

     _ <- if ghcMode == "WITH_GHC=TRUE\n"
             then element output # set UI.text ("Started interpreter using local GHC installation \n" ++ stdout)
             else element output # set UI.text ("Started interpreter with packaged GHC \n" ++ stdout)

     if ghcMode == "WITH_GHC=TRUE\n"
        then void $ liftIO $ forkIO $ startHintJob True str bootDefs defsMV mMV rMV -- True = safe
        else void $ liftIO $ forkIO $ startHintJob False str bootDefs defsMV mMV rMV

     let env = Env win str output high pats mMV rMV defsMV
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
     UI.getBody win #. "CodeMirror cm-s-theme"
                    # set UI.style [("background-color","black")]
                    #+
                       [element display
                       ,UI.div #. "editor" #+ [UI.div #. "main" #+ [element ctrl]]
                       ,element output
                       ,element svg
                       ,element fileInput
                       ,element inputScript
                       ,element settings
                       ,element makeCtrlEditor
                       ,element recorder
                       ]


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
           mMV = hintM env
           rMV = hintR env
           defsMV = eDefs env
           p = streamReplace str
       contentsControl <- liftUI editorValueControl
       line <- liftUI getCursorLine
       let bs = getBlocks contentsControl
           blockMaybe = if lineBool then getLineContent line (linesNum contentsControl) else getBlock line bs
       case blockMaybe of
           Nothing -> void $ liftUI $ element out # set UI.text "Failed to get Block"
           Just (Block blockLineStart blockLineEnd block) -> do
                   case parse parseCommand "" block of
                         Left e -> errorUI $ show e
                         Right command -> case command of

                                         (H name' s (ln,ch)) -> do
                                                 let name = ID name'
                                                 liftIO $ putMVar mMV $ MHigh s
                                                 res <- liftIO $ takeMVar rMV
                                                 case res of
                                                     RHigh pat -> do
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
                                                     RError e -> errorUI e
                                                     _ -> return ()

                                         (Other s)   -> do
                                                 liftIO $ putMVar mMV $ MStat s
                                                 res <- liftIO $ takeMVar rMV
                                                 case res of
                                                   RStat "()" -> successUI
                                                   RStat (['\"','d',num,'\"']) -> do
                                                                    successUI >> (outputUI "")
                                                                    patStates <- liftIO $ takeMVar patStatesMVar
                                                                    let newPatS = Map.insert (read [num]) (PS (read [num]) False False) patStates
                                                                    liftIO $ putMVar patStatesMVar $ newPatS
                                                   RStat outputString -> successUI >> (outputUI outputString)
                                                   RError e -> errorUI e
                                                   _ -> return ()

                                         (T s)       -> do
                                                    liftIO $ putMVar mMV $ MType s
                                                    res <- liftIO $ takeMVar rMV
                                                    case res of
                                                      (RType t) -> successUI >> (outputUI t)
                                                      (RError e) -> errorUI e
                                                      _ -> return ()

                                         (Def s)     -> do
                                                 liftIO $ putMVar mMV $ MDef s
                                                 res <- liftIO $ takeMVar rMV
                                                 case res of
                                                   (RDef d) -> do
                                                     successUI
                                                     outputUI ""
                                                     defs <- liftIO $ takeMVar defsMV
                                                     -- could be smarter to create less redundancy
                                                     case elem d defs of
                                                          True -> liftIO $ putMVar defsMV defs
                                                          False -> liftIO $ putMVar defsMV (defs ++ [d])
                                                   (RError e) -> errorUI e
                                                   _ -> return ()

                                         (Hush)      -> successUI >> (liftIO $ hush str patStatesMVar highStatesMVar)

            where successUI = liftUI $ flashSuccess blockLineStart blockLineEnd
                  errorUI err = (liftUI $ flashError blockLineStart blockLineEnd) >> (void $ liftUI $ element out # C.set UI.text err)
                  outputUI o = void $ liftUI $ element out # set UI.text o
