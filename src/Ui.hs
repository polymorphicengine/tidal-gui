module Ui where

import Graphics.UI.Threepenny.Core as C hiding (text)
import qualified Graphics.UI.Threepenny as UI

import Sound.Tidal.Context hiding (solo, (#))
import Sound.Tidal.ID

import Control.Concurrent.MVar  (tryPutMVar, takeMVar, readMVar, modifyMVar_)
import Control.Monad (void)

import Data.Map as Map  (insert, lookup, empty, toList, delete, fromList)

import Foreign.JavaScript (JSObject)
import Control.Concurrent (threadDelay)


-- this displays values that are possibly continously changing without any interaction of the user (currently only cps, but could also display values in busses etc.)
displayLoop :: Window -> Element -> Stream -> IO ()
displayLoop win display stream = do
                          valueMap <- liftIO $ readMVar (sStateMV stream)
                          let filtered = Prelude.filter (\(x,_) -> case x of '_':y -> if y == "cps" then True else False; _ -> True) (toList valueMap)
                          els <- runUI win $ valMapEl (Map.fromList filtered)
                          void $ runUI win $ element display # set UI.children els
                          runUI win $ runFunction $ ffi "requestAnimationFrame(displayLoop)"

-- this function should be called after any event that might change the playstate of the stream
updateDisplay ::  Stream -> UI ()
updateDisplay stream = do
                  display <- getDisplayElP
                  playMap <- liftIO $ readMVar (sPMapMV stream)
                  els <- playMapEl stream playMap
                  void $ element display # set UI.children els

getDisplayElP :: UI Element
getDisplayElP = do
         win <- askWindow
         elMay <- getElementById win "displayP"
         case elMay of
           Nothing -> error "can't happen"
           Just el -> return el

showPlayState :: PlayState -> String
showPlayState (PlayState _ mt solo _) | mt = "m "
                                      | solo = "s "
                                      | otherwise = "p "

showPlayMap :: PlayMap -> String
showPlayMap pMap = concat [ i ++ ": " ++ showPlayState ps ++ " " | (i,ps) <- pList]
                where pList = toList pMap

playStateEl :: Stream -> (PatId, PlayState) -> UI Element
playStateEl str (idd,ps) = do
            el <- UI.pre # set UI.text (idd ++ ": " ++ showPlayState ps)
            on UI.click el $ \_ -> (liftIO $ removeP str (ID idd)) >> updateDisplay str
            return el

playMapEl :: Stream -> PlayMap -> UI [Element]
playMapEl str pm = sequence $ fmap (playStateEl str) (toList pm)

valEl :: (String, Value) -> UI Element
valEl (idd,ps) = do
            el <- UI.pre # set UI.text (idd ++ ": " ++ show ps ++ " ")
            return el

valMapEl :: ValueMap -> UI [Element]
valMapEl vm = sequence $ fmap valEl (toList vm)

removeV :: Stream -> String -> IO ()
removeV str i = do
          vMap <- liftIO $ readMVar (sStateMV str)
          let newVMap = Map.delete i vMap
          void $ tryPutMVar (sStateMV str) newVMap

removeP :: Stream -> ID -> IO ()
removeP str i = do
          pState <- takeMVar $ sPMapMV str
          let newPState = Map.delete (fromID i) pState
          void $ tryPutMVar (sPMapMV str) newPState

muteP :: Stream -> ID -> IO ()
muteP str i = do
            pState <- takeMVar $ sPMapMV str
            case Map.lookup (fromID i) pState of
                  Just (p@(PlayState _ mt _ _)) -> do
                          let newPState = Map.insert (fromID i) (p {mute = not mt}) pState
                          void $ tryPutMVar (sPMapMV str) newPState
                  Nothing -> void $ tryPutMVar (sPMapMV str) pState

hush :: Stream -> IO ()
hush str  = modifyMVar_ (sPMapMV str) (\_ -> return Map.empty)
--flash on evaluation

checkUndefined :: ToJS a => a -> UI String
checkUndefined cm = callFunction $ ffi "(function (a) { if (typeof a === 'undefined' || a === null) {return \"yes\";} else { return \"no\"; } })(%1)" cm

highlightBlock :: JSObject -> Int -> Int -> String -> UI JSObject
highlightBlock cm lineStart lineEnd color = do
                                        undef <- checkUndefined cm
                                        case undef of
                                          "no" -> callFunction $ ffi "((%1).markText({line: %2, ch: 0}, {line: %3, ch: 0}, {css: %4}))" cm lineStart lineEnd color
                                          _ -> callFunction $ ffi "return {}"

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
