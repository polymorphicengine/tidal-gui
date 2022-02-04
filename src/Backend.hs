{-# LANGUAGE FlexibleInstances, OverloadedStrings #-}
module Backend where

import System.FilePath  (dropFileName)
import System.Environment (getExecutablePath)
import System.Directory (getDirectoryContents)

import Control.Concurrent (forkIO)
import Control.Concurrent.MVar  (newEmptyMVar, MVar, putMVar, newMVar, takeMVar)
import Control.Monad  (void)
import Control.Monad.Reader (ReaderT, runReaderT, ask)


import Sound.Tidal.Context as T hiding (mute,solo,(#),s)

import Text.Parsec  (parse)

import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core as C hiding (text)


import Parse
import Ui
import Hint

data Env = Env {windowE :: Window
                 ,streamE :: Stream
                 ,hintM :: MVar InterpreterMessage
                 ,hintR :: MVar InterpreterResponse
                 ,eDefs :: MVar [String]
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
      let str = streamE env
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
                                                  RStat (['\"','d',_,'\"']) -> successUI >> (outputUI "")
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

                                        (Hush)      -> successUI >> (liftIO $ hush str)

           where successUI = liftUI $ flashSuccess blockLineStart blockLineEnd
                 errorUI err = (liftUI $ flashError blockLineStart blockLineEnd) >> (void $ liftUI $ element out # set UI.text err)
                 outputUI o = void $ liftUI $ element out # set UI.text o

setupBackend :: Stream -> String -> UI ()
setupBackend str stdout = do

       win <- askWindow
       env <- startInterpreter str stdout

       disp <- getDisplayEl

       createHaskellFunction "displayLoop" (displayLoop win disp str)
       void $ liftIO $ forkIO $ runUI win $ runFunction $ ffi "requestAnimationFrame(displayLoop)"

       createShortcutFunctions str

       createHaskellFunction "evaluateBlock" (runReaderT (interpretCommands False) env)
       createHaskellFunction "evaluateLine" (runReaderT (interpretCommands True) env)


getBootDefs :: IO [String]
getBootDefs = do
       execPath <- liftIO $ dropFileName <$> getExecutablePath
       userDefsPaths <- liftIO $ getDirectoryContents $ execPath ++ "static/definitions/"
       bootDefs <- liftIO $ sequence $ map (\x -> readFile $ execPath ++ "static/definitions/" ++ x) $ filter (\s -> s /= "." && s /= "..") userDefsPaths
       return bootDefs


startInterpreter :: Stream -> String -> UI Env
startInterpreter str stdout = do

           win <- askWindow
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

           return $ Env win str mMV rMV defsMV

createShortcutFunctions :: Stream -> UI ()
createShortcutFunctions str = do

                       createHaskellFunction "hush" (hush str)

                       createHaskellFunction "muteP1" (muteP str 1)
                       createHaskellFunction "muteP2" (muteP str 2)
                       createHaskellFunction "muteP3" (muteP str 3)
                       createHaskellFunction "muteP4" (muteP str 4)
                       createHaskellFunction "muteP5" (muteP str 5)
                       createHaskellFunction "muteP6" (muteP str 6)
                       createHaskellFunction "muteP7" (muteP str 7)
                       createHaskellFunction "muteP8" (muteP str 8)
                       createHaskellFunction "muteP9" (muteP str 9)

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

editorValueControl :: UI String
editorValueControl = callFunction $ ffi "controlEditor.getValue()"

createHaskellFunction name fn = do
  handler <- ffiExport fn
  runFunction $ ffi ("window." ++ name ++ " = %1") handler

getCursorLine :: UI Int
getCursorLine = callFunction $ ffi "(controlEditor.getCursor()).line"
