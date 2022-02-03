module Ui where

import Graphics.UI.Threepenny.Core as C hiding (text)
import qualified Graphics.UI.Threepenny as UI

import Sound.Tidal.Context hiding (solo, (#))
import Sound.Tidal.ID

import Control.Concurrent.MVar  (MVar, tryPutMVar, tryTakeMVar, readMVar, takeMVar, putMVar)
import Control.Monad (void)

import Data.Map as Map  (Map, insert, fromList, assocs, lookup, empty, toList)

import Foreign.JavaScript (JSObject)
import Control.Concurrent (threadDelay)


data PatternState = PS {psChan :: Int,
                        sMuted :: Bool,
                        sSolo :: Bool
                       } deriving Show

type PatternStates = Map Int PatternState


displayLoop :: Window -> Element -> Stream -> IO ()
displayLoop win display stream = do
                          valueMap <- liftIO $ readMVar (sStateMV stream)
                          playMap <- liftIO $ readMVar (sPMapMV stream)
                          let cpsDisplay = case Map.lookup "_cps" valueMap of
                                          Nothing -> ""
                                          Just x -> show x
                          void $ runUI win $ element display # set UI.text ("cps: " ++ cpsDisplay ++ "\n" ++ showPlayMap playMap)
                          runUI win $ runFunction $ ffi "requestAnimationFrame(displayLoop)"



showPlayState :: PlayState -> String
showPlayState (PlayState _ mt solo _) | mt = "muted"
                                      | solo = "solo"
                                      | otherwise = "playing"

showPlayMap :: PlayMap -> String
showPlayMap pMap = concat [ i ++ ": " ++ showPlayState ps ++ " " | (i,ps) <- pList]
                where pList = toList pMap


--get the contents of the codeMirror editor
editorValueControl :: UI String
editorValueControl = callFunction $ ffi "controlEditor.getValue()"

createHaskellFunction name fn = do
   handler <- ffiExport fn
   runFunction $ ffi ("window." ++ name ++ " = %1") handler

getCursorLine :: UI Int
getCursorLine = callFunction $ ffi "(controlEditor.getCursor()).line"

hush :: Stream -> MVar PatternStates  -> IO ()
hush str patStatesMVar = do
              _ <- takeMVar (sPMapMV str)
              _ <- putMVar (sPMapMV str) Map.empty
              _ <- takeMVar patStatesMVar
              _ <- putMVar patStatesMVar $ Map.empty
              streamHush str

muteP :: Stream -> MVar PatternStates -> ID -> IO ()
muteP str patStatesMVar i = do
            patStates <- tryTakeMVar patStatesMVar
            case patStates of
                  Just pats -> do
                      case Map.lookup key pats of
                        Just p -> do
                          let newPatS = Map.insert key (p {sMuted = not (sMuted p)}) pats
                          if sMuted p then streamUnmute str i else streamMute str i
                          _ <- tryPutMVar patStatesMVar $ newPatS
                          return ()
                        Nothing -> (void $ tryPutMVar patStatesMVar $ pats)
                  Nothing -> return ()
              where key = read $ fromID i


--flash on evaluation

highlightBlock :: Int -> Int -> String -> UI JSObject
highlightBlock lineStart lineEnd color = callFunction $ ffi "(controlEditor.markText({line: %1, ch: 0}, {line: %2, ch: 0}, {css: %3}))" lineStart lineEnd color

unHighlight :: JSObject -> UI ()
unHighlight mark = runFunction $ ffi "if (typeof %1 !== 'undefined'){%1.clear()};" mark

flashSuccess :: Int -> Int -> UI ()
flashSuccess lineStart lineEnd = do
                            mark <- highlightBlock lineStart (lineEnd + 1) "background-color: green"
                            liftIO $ threadDelay 100000
                            unHighlight mark
                            flushCallBuffer

flashError :: Int -> Int -> UI ()
flashError lineStart lineEnd = do
                            mark <- highlightBlock lineStart (lineEnd + 1) "background-color: red"
                            liftIO $ threadDelay 100000
                            unHighlight mark
                            flushCallBuffer
