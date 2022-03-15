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

import Data.IORef (IORef, readIORef, modifyIORef, newIORef)

import Foreign.JavaScript (JSObject)


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

interpretCommands :: JSObject -> Bool -> ReaderT Env IO ()
interpretCommands cm lineBool = do
      env <- ask
      let str = streamE env
          mMV = hintM env
          rMV = hintR env
          defsMV = eDefs env
      undef <- liftUI $ checkUndefined cm
      case undef of
        "yes" -> return ()
        _ -> do
              contentsControl <- liftUI $ getValue cm
              line <- liftUI $ getCursorLine cm
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

                   where successUI = liftUI $ flashSuccess cm blockLineStart blockLineEnd
                         errorUI err = (liftUI $ flashError cm blockLineStart blockLineEnd) >> (void $ liftUI $ element out # set UI.text err)
                         outputUI o = void $ liftUI $ element out # set UI.text o

setupBackend :: Stream -> String -> UI ()
setupBackend str stdout = do

       win <- askWindow
       env <- startInterpreter str stdout

       disp <- getDisplayEl

       createHaskellFunction "displayLoop" (displayLoop win disp str)
       void $ liftIO $ forkIO $ runUI win $ runFunction $ ffi "requestAnimationFrame(displayLoop)"

       createHaskellFunction "evaluateBlock" (\cm -> runReaderT (interpretCommands cm False) env)
       createHaskellFunction "evaluateLine" (\cm -> runReaderT (interpretCommands cm True) env)

       on disconnect win $ \_ -> (runFunction $ ffi "saveFile()")


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

           createHaskellFunction "replaceWordByDef" (\cm -> runUI win $ replaceWordByDef mMV rMV cm)
           return $ Env win str mMV rMV defsMV

createShortcutFunctions :: Stream -> Element -> UI ()
createShortcutFunctions str mainEditor = do
                       editorsRef <- liftIO $ newIORef [mainEditor]
                       win <- askWindow
                       createHaskellFunction "addEditor" (runUI win $ addEditor editorsRef)
                       createHaskellFunction "removeEditor" (runUI win $ removeEditor editorsRef)

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

getValue :: ToJS a => a -> UI String
getValue cm = callFunction $ ffi "getV(%1)" cm

createHaskellFunction name fn = do
  handler <- ffiExport fn
  runFunction $ ffi ("window." ++ name ++ " = %1") handler

getCursorLine :: ToJS a => a -> UI Int
getCursorLine cm = callFunction $ (ffi "getCursorLine(%1)") cm

makeEditor :: String -> UI ()
makeEditor i = runFunction $ ffi $ "CodeMirror.fromTextArea(document.getElementById('" ++ i ++ "'), editorSettings);"

addEditor :: IORef [Element]  -> UI ()
addEditor ref = do
        old <- liftIO $ readIORef ref
        let x = show $ length old
        editor <- UI.textarea # set (attr "id") ("editor" ++ x)
        d <- UI.div #. "main" #+ [element editor] # set UI.style [("flex-grow","8")]
        liftIO $ modifyIORef ref (\xs -> xs ++ [d])
        redoEditorLayout ref
        makeEditor ("editor" ++ x)

removeEditor :: IORef [Element] -> UI ()
removeEditor ref = do
          xs <- liftIO $ readIORef ref
          case length xs == 1 of
            True -> return ()
            False -> do
                liftIO $ modifyIORef ref (\ys -> take (length xs - 1) ys)
                redoEditorLayout ref

redoEditorLayout :: IORef [Element] -> UI ()
redoEditorLayout ref = do
            win <- askWindow
            eds <- liftIO $ readIORef ref
            editorsMay <- getElementById win "editors"
            case editorsMay of
              Nothing -> error "cant happen"
              Just editors -> void $ element editors # set UI.children eds


replaceWordByDef :: MVar InterpreterMessage -> MVar InterpreterResponse -> JSObject -> UI ()
replaceWordByDef mMV rMV cm = do
      loc <- (callFunction $ ffi "(%1).findWordAt((%1).getCursor())" cm) :: UI JSObject
      word <- (callFunction $ (ffi "(%1).getRange((%2).anchor, (%2).head)" cm loc)) :: UI String
      liftIO $ putMVar mMV $ MStat ("return $ " ++ word)
      res <- liftIO $ takeMVar rMV
      case res of
        RStat outputString -> runFunction $ ffi ("(%1).replaceRange(" ++ outputString ++ ", (%2).anchor, (%2).head)") cm loc
        _ -> return ()

noDoubleQuotes :: String -> String
noDoubleQuotes = init . tail
