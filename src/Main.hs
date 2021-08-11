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

import qualified Hoogle as Hoo

import Parse
import Highlight
import Ui
import Hint


main :: IO ()
main = do
    execPath <- dropFileName <$> getExecutablePath
    str <- T.startTidal (T.superdirtTarget {oLatency = 0.1, oAddress = "127.0.0.1", oPort = 57120}) (T.defaultConfig {cVerbose = True, cFrameTimespan = 1/20})
    startGUI C.defaultConfig {
          jsStatic = Just $ execPath ++ "static",
          jsCustomHTML     = Just "tidal.html"
        } $ setup str


data Env = Env {windowE :: Window
               ,streamE :: Stream
               ,outputE :: Element
               ,patH :: MVar HighlightStates
               ,patS :: MVar PatternStates
               ,hintM :: MVar InterpreterMessage
               ,hintR :: MVar InterpreterResponse
               ,eDefs :: MVar [String]
               }


setup :: Stream -> Window -> UI ()
setup str win = void $ do
     --setup GUI
     void $ return win # set title "Tidal"

     UI.addStyleSheet win "tidal.css"
     UI.addStyleSheet win "theme.css"

     setCallBufferMode NoBuffering -- important for highlighting

     ctrl <- UI.textarea # set (attr "id") "control-editor"

     output <- UI.pre #. "outputBox"
                      #+ [ string "output goes here" ]
     display <- UI.pre #. "displayBox"

     load <- UI.input
                  # set (attr "type") "file"
                  # set (attr "id") "fileInput"
                  # set (attr "onchange") "controlLoadFile()"

     save <- UI.button
                  # set UI.text "Save file"
                  # set (attr "onclick") "controlSaveFile()"

     execPath <- liftIO $ dropFileName <$> getExecutablePath
     tidalKeys <- liftIO $ readFile $ execPath ++ "static/tidalConfig.js"
     ghcMode <- liftIO $ readFile $ execPath ++ "static/ghc_mode.txt"
     --recorderScript <- liftIO $ readFile $ execPath ++ "static/codemirror/cm-record.js"
     boot <- liftIO $ readFile $ execPath ++ "static/bootDefs.hs"

     settings <- mkElement "script" # set UI.text tidalKeys
     --recorder <- mkElement "script" # set UI.text recorderScript

     makeCtrlEditor <- mkElement "script"
                       # set UI.text "const controlEditor = CodeMirror.fromTextArea(document.getElementById('control-editor'), controlEditorSettings);"

     --highlight (experimental)
     high <- liftIO newEmptyMVar
     pats <- liftIO $ newMVar Map.empty
     mMV <- liftIO newEmptyMVar
     rMV <- liftIO newEmptyMVar
     defsMV <- liftIO $ newMVar []

     void $ liftIO $ forkIO $ highlightLoop [] str win high
     void $ liftIO $ forkIO $ displayLoop win display str

     _ <- if ghcMode == "WITH_GHC=TRUE\n"
             then element output # set UI.text "Started interpreter using local GHC installation"
             else element output # set UI.text "Started interpreter with packaged GHC"

     if ghcMode == "WITH_GHC=TRUE\n"
        then void $ liftIO $ forkIO $ startHintJob True str boot defsMV mMV rMV -- True = safe
        else void $ liftIO $ forkIO $ startHintJob False str boot defsMV mMV rMV

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
     UI.getBody win #. "CodeMirror cm-s-theme" #+
                       [element display
                       ,UI.div #. "editor" #+ [UI.div #. "main" #+ [element ctrl]]
                       ,element load
                       ,element save
                       ,element output
                       ,element settings
                       ,element makeCtrlEditor
                      -- ,element recorder
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

                                         (H name s (ln,ch)) -> do
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
                                                     liftIO $ putMVar defsMV (defs ++ [d])
                                                   (RError e) -> errorUI e
                                                   _ -> return ()

                                         (Hush)      -> successUI >> (liftIO $ hush str patStatesMVar highStatesMVar)

                                         (Hoogle s)  -> liftUI $ hoogleJob s out

            where successUI = liftUI $ flashSuccess blockLineStart blockLineEnd
                  errorUI err = (liftUI $ flashError blockLineStart blockLineEnd) >> (void $ liftUI $ element out # C.set UI.text err)
                  outputUI o = void $ liftUI $ element out # set UI.text o


hoogleJob :: String -> Element -> UI ()
hoogleJob input out = do
  execPath <- liftIO $ dropFileName <$> getExecutablePath
  targ <- liftIO $ Hoo.withDatabase (execPath ++ "static/tidal.hoo") (hooSearch input)
  case targ of
    Left t -> void $ element out # set UI.text (Hoo.targetInfo t)
    Right _ -> void $ element out # set UI.text "Nothing found"

hooSearch :: String -> Hoo.Database -> IO (Either Hoo.Target ())
hooSearch input dat | search == [] = return $ Right ()
                    | otherwise = return $ Left $ head search
                    where search = Hoo.searchDatabase dat input
