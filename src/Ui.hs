module Ui where

import Graphics.UI.Threepenny.Core as C hiding (text)
import qualified Graphics.UI.Threepenny as UI

import Sound.Tidal.Context hiding (solo, (#))
import Sound.Tidal.ID

import Control.Concurrent.MVar  (tryPutMVar, takeMVar, readMVar, modifyMVar_)
import Control.Monad (void)

import Data.Map as Map  (insert, lookup, empty, toList)

import Foreign.JavaScript (JSObject)
import Control.Concurrent (threadDelay)



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


hush :: Stream -> IO ()
hush str  = modifyMVar_ (sPMapMV str) (\_ -> return Map.empty)

muteP :: Stream -> ID -> IO ()
muteP str i = do
            pState <- takeMVar $ sPMapMV str
            case Map.lookup (fromID i) pState of
                  Just (p@(PlayState _ mt _ _)) -> do
                          let newPState = Map.insert (fromID i) (p {mute = not mt}) pState
                          void $ tryPutMVar (sPMapMV str) newPState
                  Nothing -> void $ tryPutMVar (sPMapMV str) pState



--flash on evaluation

highlightBlock :: JSObject -> Int -> Int -> String -> UI JSObject
highlightBlock cm lineStart lineEnd color = callFunction $ ffi "((%1).markText({line: %2, ch: 0}, {line: %3, ch: 0}, {css: %4}))" cm lineStart lineEnd color

unHighlight :: JSObject -> UI ()
unHighlight mark = runFunction $ ffi "if (typeof %1 !== 'undefined'){%1.clear()};" mark

flashSuccess :: JSObject -> Int -> Int -> UI ()
flashSuccess cm lineStart lineEnd = do
                            mark <- highlightBlock cm lineStart (lineEnd + 1) "background-color: green"
                            liftIO $ threadDelay 100000
                            unHighlight mark
                            flushCallBuffer

flashError :: JSObject -> Int -> Int -> UI ()
flashError cm lineStart lineEnd = do
                            mark <- highlightBlock cm lineStart (lineEnd + 1) "background-color: red"
                            liftIO $ threadDelay 100000
                            unHighlight mark
                            flushCallBuffer
