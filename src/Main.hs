{-# LANGUAGE FlexibleInstances, OverloadedStrings #-}

import System.FilePath  (dropFileName)
import System.Environment (getExecutablePath)
import System.Directory (getDirectoryContents)

import Control.Concurrent (forkIO)
import Control.Concurrent.MVar  (newEmptyMVar, MVar, putMVar, newMVar, takeMVar)
import Control.Monad  (void)
import Control.Monad.Reader (ReaderT, runReaderT, ask)

import Data.Map as Map (insert, empty)


import Sound.Tidal.Context as T hiding (mute,solo,(#),s)

import Text.Parsec  (parse)

import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core as C hiding (text)

import System.IO.Silently

import Parse
import Ui
import Hint



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

     editor <- UI.textarea # set (attr "id") "control-editor"

     output <- UI.pre # set UI.id_ "output"
                      #. "outputBox"
                      #+ [ string "output goes here" ]
                      # set style [("font-size","3vh")]
     display <- UI.pre # set UI.id_ "display"
                       #. "displayBox"
                       # set style [("font-size","3vh")]

     fileInput <- UI.input # set UI.id_ "fileInput"
                           # set UI.type_ "file"
                           # set style [("display","none")]

     makeCtrlEditor <- mkElement "script"
                       # set UI.text "const controlEditor = CodeMirror.fromTextArea(document.getElementById('control-editor'), controlEditorSettings);"


     body <- UI.getBody win

     -- put elements on body
     _ <- (element body) #. "CodeMirror cm-s-theme"
                    # set UI.style [("background-color","black")]
                    #+  [element display
                        ,UI.div #. "editor" #+ [UI.div #. "main" #+ [element editor]]
                        ,element output
                        ]

     setupBackend str stdout

     (element body) #+  [element fileInput
                        ,inputScript
                        ,tidalSettings
                        ,element makeCtrlEditor
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
       let str = streamE env
           patStatesMVar = patS env
           mMV = hintM env
           rMV = hintR env
           defsMV = eDefs env
       contentsControl <- liftUI editorValueControl
       line <- liftUI getCursorLine
       out <- liftUI getOutputEl
       let bs = getBlocks contentsControl
           blockMaybe = if lineBool then getLineContent line (linesNum contentsControl) else getBlock line bs
       case blockMaybe of
           Nothing -> void $ liftUI $ element out # set UI.text "Failed to get Block"
           Just (Block blockLineStart blockLineEnd block) -> do
                   case parse parseCommand "" block of
                         Left e -> errorUI $ show e
                         Right command -> case command of

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

                                         (Hush)      -> successUI >> (liftIO $ hush str patStatesMVar)

            where successUI = liftUI $ flashSuccess blockLineStart blockLineEnd
                  errorUI err = (liftUI $ flashError blockLineStart blockLineEnd) >> (void $ liftUI $ element out # C.set UI.text err)
                  outputUI o = void $ liftUI $ element out # set UI.text o

setupBackend :: Stream -> String -> UI ()
setupBackend str stdout = do

        win <- askWindow
        env <- startInterpreter str stdout

        disp <- getDisplayEl

        createHaskellFunction "displayLoop" (displayLoop win disp str)
        void $ liftIO $ forkIO $ runUI win $ runFunction $ ffi "requestAnimationFrame(displayLoop)"

        createShortcutFunctions env


getBootDefs :: IO [String]
getBootDefs = do
        execPath <- liftIO $ dropFileName <$> getExecutablePath
        userDefsPaths <- liftIO $ getDirectoryContents $ execPath ++ "static/definitions/"
        bootDefs <- liftIO $ sequence $ map (\x -> readFile $ execPath ++ "static/definitions/" ++ x) $ filter (\s -> s /= "." && s /= "..") userDefsPaths
        return bootDefs


startInterpreter :: Stream -> String -> UI Env
startInterpreter str stdout = do

            win <- askWindow
            pats <- liftIO $ newMVar Map.empty
            mMV <- liftIO newEmptyMVar
            rMV <- liftIO newEmptyMVar
            defsMV <- liftIO $ newMVar []
            bootDefs <- liftIO getBootDefs
            execPath <- liftIO $ dropFileName <$> getExecutablePath
            ghcMode <- liftIO $ readFile $ execPath ++ "static/ghc_mode.txt"


            if ghcMode == "WITH_GHC=TRUE\n"
               then void $ liftIO $ forkIO $ startHintJob True str bootDefs defsMV mMV rMV -- True = safe
               else void $ liftIO $ forkIO $ startHintJob False str bootDefs defsMV mMV rMV

            out <- getOutputEl
            _ <- if ghcMode == "WITH_GHC=TRUE\n"
                     then element out # set UI.text ("Started interpreter using local GHC installation \n" ++ stdout)
                     else element out # set UI.text ("Started interpreter with packaged GHC \n" ++ stdout)

            return $ Env win str pats mMV rMV defsMV

createShortcutFunctions :: Env -> UI ()
createShortcutFunctions env = do
                        let str = streamE env
                            pats = patS env

                        createHaskellFunction "evaluateBlock" (runReaderT (interpretCommands False) env)
                        createHaskellFunction "evaluateLine" (runReaderT (interpretCommands True) env)
                        createHaskellFunction "hush" (hush str pats)

                        createHaskellFunction "muteP1" (muteP str pats 1)
                        createHaskellFunction "muteP2" (muteP str pats 2)
                        createHaskellFunction "muteP3" (muteP str pats 3)
                        createHaskellFunction "muteP4" (muteP str pats 4)
                        createHaskellFunction "muteP5" (muteP str pats 5)
                        createHaskellFunction "muteP6" (muteP str pats 6)
                        createHaskellFunction "muteP7" (muteP str pats 7)
                        createHaskellFunction "muteP8" (muteP str pats 8)
                        createHaskellFunction "muteP9" (muteP str pats 9)

getOutputEl :: UI Element
getOutputEl = do
          win <- askWindow
          elMay <- getElementById win "output"
          case elMay of
            Nothing -> error "can't happen"
            Just el -> return el

getDisplayEl :: UI Element
getDisplayEl = do
          win <- askWindow
          elMay <- getElementById win "display"
          case elMay of
            Nothing -> error "can't happen"
            Just el -> return el

tidalSettings :: UI Element
tidalSettings = do
          execPath <- liftIO $ dropFileName <$> getExecutablePath
          tidalKeys <- liftIO $ readFile $ execPath ++ "static/tidalConfig.js"
          settings <- mkElement "script" # set UI.text tidalKeys
          return settings

inputScript :: UI Element
inputScript = mkElement "script" # set UI.text  "document.getElementById(\"fileInput\").onchange = e => { var file = e.target.files[0]; var reader = new FileReader();reader.readAsText(file,'UTF-8');reader.onload = function() {controlEditor.getDoc().setValue(reader.result);};}"
