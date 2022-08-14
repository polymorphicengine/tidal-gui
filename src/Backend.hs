{-# LANGUAGE FlexibleInstances, OverloadedStrings #-}
module Backend where

import System.FilePath  (dropFileName)
import System.Environment (getExecutablePath)
import System.Directory (listDirectory, doesDirectoryExist)

import Control.Concurrent (forkIO)
import Control.Concurrent.MVar  (newEmptyMVar, MVar, putMVar, takeMVar)
import Control.Monad  (void)
import Control.Monad.Reader (ReaderT, runReaderT, ask)
import Control.Exception  (SomeException)
import Control.Monad.Catch (try)


import Sound.Tidal.Context as T hiding (mute,solo,(#),s)
import Sound.Tidal.ID

import Sound.OSC.FD as O

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
                 }



-- to combine UI and IO actions with an environment
instance MonadUI (ReaderT Env IO) where
  liftUI m = do
            env <- ask
            let win = windowE env
            liftIO $ runUI win m

interpretCommands :: JSObject -> Bool -> ReaderT Env IO ()
interpretCommands cm lineBool = do
                  line <- liftUI $ getCursorLine cm
                  interpretCommandsLine cm lineBool line

interpretCommandsLine :: JSObject -> Bool -> Int -> ReaderT Env IO ()
interpretCommandsLine cm lineBool line = do
      env <- ask
      let str = streamE env
          mMV = hintM env
          rMV = hintR env
      undef <- liftUI $ checkUndefined cm
      case undef of
        "yes" -> return ()
        _ -> do
              contentsControl <- liftUI $ getValue cm
              out <- liftUI getOutputEl
              let bs = getBlocks contentsControl
                  blockMaybe = if lineBool then getLineContent line (linesNum contentsControl) else getBlock line bs
              case blockMaybe of
                  Nothing -> void $ liftUI $ element out # set UI.text "Failed to get Block"
                  Just (Block blockLineStart blockLineEnd block) -> do
                          case parse parseCommand "" block of
                                Left e -> errorUI $ show e
                                Right command -> case command of

                                                (Statement s)   -> do
                                                        -- evaluate the given expression, if a string is returned, print it to the console
                                                        liftIO $ putMVar mMV $ MStat s
                                                        res <- liftIO $ takeMVar rMV
                                                        case res of
                                                          RStat (Just "()") -> successUI >> outputUI ""
                                                          RStat (Just outputString) -> successUI >> (outputUI outputString)
                                                          RStat Nothing -> successUI >> outputUI ""
                                                          RError e -> errorUI e
                                                          _ -> return ()

                                                (T s)       -> do
                                                           -- ask the interpreter for the type of the given expression
                                                           liftIO $ putMVar mMV $ MType s
                                                           res <- liftIO $ takeMVar rMV
                                                           case res of
                                                             (RType t) -> successUI >> (outputUI t)
                                                             (RError e) -> errorUI e
                                                             _ -> return ()
                                                (M s)   -> do
                                                         -- evaluate the given expression expecting a string, which will replace the line
                                                         liftIO $ putMVar mMV $ MStat s
                                                         res <- liftIO $ takeMVar rMV
                                                         case res of
                                                           RStat (Just "()") -> errorUI "A makro has to return a string"
                                                           RStat (Just outputString) -> successUI >> (liftUI $ runFunction $ ffi ("(%1).replaceRange(" ++ outputString ++ ", {line: (%2), ch: 0}, {line: (%2), ch: 50})") cm line)
                                                           RStat Nothing -> errorUI "A makro has to return a string"
                                                           RError e -> errorUI e
                                                           _ -> return ()
                                                (Hush)     -> successUI >> (liftIO $ hush str)
                                                (Conf DefPath s) -> do
                                                          x <- liftUI $ setDefPath s
                                                          case x of
                                                              Left _ -> errorUI "Perhaps you don't have authority to write to the config file?\nTry running with sudo or admin privileges"
                                                              Right True -> successUI >> (outputUI $ "Successfully set path to: " ++ s)
                                                              Right False -> errorUI "This path doesn't seem to exist!"
                                                (Listen s i) -> (liftUI $ void $ liftIO $ forkIO $ listen s i env) >> successUI >> outputUI ("Listening on port " ++ s ++ ":" ++ show i)
                                                (Hydra s) -> do
                                                        x <- liftUI $ hydraJob s
                                                        case x == "" of
                                                          True -> successUI >> outputUI ""
                                                          False -> errorUI (show x)
                   where successUI = liftUI $ flashSuccess cm blockLineStart blockLineEnd
                         errorUI err = (liftUI $ flashError cm blockLineStart blockLineEnd) >> (void $ liftUI $ element out # set UI.text err)
                         outputUI o = void $ liftUI $ element out # set UI.text o
              liftUI $ updateDisplay str

setupBackend :: Stream -> String -> UI ()
setupBackend str stdout = do

       win <- askWindow
       env <- startInterpreter str stdout

       createHaskellFunction "evaluateBlock" (\cm -> runReaderT (interpretCommands cm False) env)
       createHaskellFunction "evaluateLine" (\cm -> runReaderT (interpretCommands cm True) env)

       createHaskellFunction "evaluateBlockLine" (\cm l -> runReaderT (interpretCommandsLine cm False l) env)
       createHaskellFunction "evaluateLineLine" (\cm l -> runReaderT (interpretCommandsLine cm True l) env)

       createHaskellFunction "displayLoop" (runUI win $ displayLoop str)
       void $ liftIO $ forkIO $ runUI win $ runFunction $ ffi "requestAnimationFrame(displayLoop)"

       on disconnect win $ \_ -> (runFunction $ ffi "saveFile()")

lookupDefPath :: UI String
lookupDefPath = callFunction $ ffi "config.defPath"

setDefPath :: String -> UI (Either SomeException Bool)
setDefPath s = do
          execPath <- liftIO $ dropFileName <$> getExecutablePath
          b <- liftIO $ doesDirectoryExist s
          case b of
            True -> try $ liftIO $ writeFile (execPath ++ "static/config.js") ("const config = {defPath:" ++ s ++ " }") >> return True
            False -> return $ Right False

getBootDefs :: UI [String]
getBootDefs = do
       bootPath <- lookupDefPath
       b <- liftIO $ doesDirectoryExist bootPath
       case b of
         False -> do
             execPath <- liftIO $ dropFileName <$> getExecutablePath
             userDefsPaths <- liftIO $ listDirectory $ execPath ++ "static/definitions/"
             bootDefs <- liftIO $ sequence $ map (\x -> readFile $ execPath ++ "static/definitions/" ++ x) userDefsPaths
             return bootDefs
         True -> do
             userDefsPaths <- liftIO $ listDirectory $ bootPath
             bootDefs <- liftIO $ sequence $ map (\x -> readFile $ bootPath ++"/"++ x) userDefsPaths
             return bootDefs


startInterpreter :: Stream -> String -> UI Env
startInterpreter str stdout = do

           win <- askWindow
           mMV <- liftIO newEmptyMVar
           rMV <- liftIO newEmptyMVar
           bootDefs <- getBootDefs
           execPath <- liftIO $ dropFileName <$> getExecutablePath
           ghcMode <- liftIO $ readFile $ execPath ++ "static/ghc_mode.txt"


           if ghcMode == "WITH_GHC=TRUE\n"
              then void $ liftIO $ forkIO $ startHintJob True str bootDefs mMV rMV -- True = safe
              else void $ liftIO $ forkIO $ startHintJob False str bootDefs mMV rMV

           out <- getOutputEl
           _ <- if ghcMode == "WITH_GHC=TRUE\n"
                    then element out # set UI.text ("Started interpreter using local GHC installation \n" ++ stdout)
                    else element out # set UI.text ("Started interpreter with packaged GHC \n" ++ stdout)

           createHaskellFunction "replaceWordByDef" (\cm -> runUI win $ replaceWordByDef mMV rMV cm)
           return $ Env win str mMV rMV

createShortcutFunctions :: Stream -> Element -> UI ()
createShortcutFunctions str mainEditor = do
                       editorsRef <- liftIO $ newIORef [mainEditor]
                       win <- askWindow
                       createHaskellFunction "addEditor" (runUI win $ addEditor editorsRef)
                       createHaskellFunction "removeEditor" (runUI win $ removeEditor editorsRef)

                       createHaskellFunction "hush" (hush str >> (runUI win $ updateDisplay str))

                       createHaskellFunction "muteP1" (muteP str 1 >> (runUI win $ updateDisplay str))
                       createHaskellFunction "muteP2" (muteP str 2 >> (runUI win $ updateDisplay str))
                       createHaskellFunction "muteP3" (muteP str 3 >> (runUI win $ updateDisplay str))
                       createHaskellFunction "muteP4" (muteP str 4 >> (runUI win $ updateDisplay str))
                       createHaskellFunction "muteP5" (muteP str 5 >> (runUI win $ updateDisplay str))
                       createHaskellFunction "muteP6" (muteP str 6 >> (runUI win $ updateDisplay str))
                       createHaskellFunction "muteP7" (muteP str 7 >> (runUI win $ updateDisplay str))
                       createHaskellFunction "muteP8" (muteP str 8 >> (runUI win $ updateDisplay str))
                       createHaskellFunction "muteP9" (muteP str 9 >> (runUI win $ updateDisplay str))

getOutputEl :: UI Element
getOutputEl = do
         win <- askWindow
         elMay <- getElementById win "output"
         case elMay of
           Nothing -> error "can't happen"
           Just el -> return el

getDisplayElV :: UI Element
getDisplayElV = do
        win <- askWindow
        elMay <- getElementById win "displayV"
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
        RStat (Just outputString) -> runFunction $ ffi ("(%1).replaceRange(" ++ outputString ++ ", (%2).anchor, (%2).head)") cm loc
        _ -> return ()

noDoubleQuotes :: String -> String
noDoubleQuotes = init . tail



--OSC

listen :: String -> Int -> Env -> IO ()
listen s listenPort env = do
            local <- udpServer s listenPort
            loopOSC env local
            where loopOSC e l = do -- wait for, read and act on OSC message
                         m <- recvMessage l
                         e' <- actOSC e m
                         loopOSC e' l

actOSC :: Env -> Maybe O.Message -> IO Env
actOSC env (Just (Message "/eval" [])) = (runUI (windowE env) $ runFunction $ ffi "evaluateBlock(document.querySelector(\"#editor0 + .CodeMirror\").CodeMirror)") >> return env
actOSC env (Just (Message "/hush" [])) = (runUI (windowE env) $ (liftIO $ hush $ streamE env) >> updateDisplay (streamE env)) >> return env
actOSC env (Just (Message "/go/line" [Int32 line])) = (runUI (windowE env) $ runFunction $ ffi "(document.querySelector(\"#editor0 + .CodeMirror\").CodeMirror).setCursor(%1)" (fromIntegral line :: Int)) >> return env
actOSC env (Just (Message "/eval/block" [Int32 line])) = (runUI (windowE env) $ runFunction $ ffi "evaluateBlockLine(document.querySelector(\"#editor0 + .CodeMirror\").CodeMirror, (%1))" ((fromIntegral line) - 1 :: Int)) >> return env
actOSC env (Just (Message "/eval/line" [Int32 line])) = (runUI (windowE env) $ runFunction $ ffi "evaluateLineLine(document.querySelector(\"#editor0 + .CodeMirror\").CodeMirror, (%1))" ((fromIntegral line) - 1 :: Int)) >> return env
actOSC env (Just (Message "/mute" [ASCII_String s])) = (runUI (windowE env) $ (liftIO $ muteP (streamE env) (ID $ ascii_to_string s)) >> updateDisplay (streamE env)) >> return env
actOSC env (Just (Message "/print" [ASCII_String s])) = (runUI (windowE env) $ getOutputEl # (set UI.text $ "Recieved message: " ++ ascii_to_string s)) >> return env
actOSC env (Just m) = (runUI (windowE env) $ getOutputEl # (set UI.text $ "Unhandeled OSC message: " ++ show m)) >> return env
actOSC env _ = return env

--hydra

startHydra :: UI ()
startHydra = runFunction $ ffi "var hydra = new Hydra({canvas: document.getElementById(\"hydraCanvas\"),detectAudio: false})"

hydraJob :: String -> UI String
hydraJob s = callFunction $ ffi "function f(x) {try{eval(x); return \"\";} catch (error) { return error.message; }}; f(%1)" s

getWindowWidth :: UI Double
getWindowWidth = callFunction $ ffi "window.innerWidth"

getWindowHeight :: UI Double
getWindowHeight = callFunction $ ffi "window.innerHeight"
