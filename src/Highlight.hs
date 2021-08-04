module Highlight where

import Sound.Tidal.Context
import Sound.Tidal.Tempo  (timeToCycles)
import Sound.OSC.FD (time)

import Graphics.UI.Threepenny.Core as C hiding (text)
import Foreign.JavaScript (JSObject)

import Control.Concurrent (threadDelay)
import Control.Concurrent.MVar  (readMVar, MVar)

import Data.Map as Map  (Map,elems)
import Data.List  ((\\))

--each event can be identified through it's whole, so it's drawn only when it first appears
type Buffer = [((Int,Int,Int), Maybe Arc)]

data HighlightState = HS {sPat :: ControlPattern
                       ,bShift :: Int
                       ,cShift :: Int
                       ,hMuted :: Bool
                       ,hSolo :: Bool
                       } deriving Show

type HighlightStates = Map String HighlightState

highlight :: (Int, Int, Int) -> UI JSObject
highlight (line, st, e) = callFunction $ ffi "(controlEditor.markText({line: %1, ch: %2}, {line: %1, ch: %3}, {css: \"background-color: blue\"}))" line st e

--safer but might leave some objects
unHighlight :: JSObject -> UI ()
unHighlight mark = runFunction $ ffi "if (typeof %1 !== 'undefined'){%1.clear()};" mark

-- unHighlight :: JSObject -> UI ()
-- unHighlight mark = runFunction $ ffi "%1.clear();" mark

locs :: Rational -> Int -> Int -> ControlPattern -> [((Int,Int,Int), Maybe Arc)]
locs t bSh cSh pat = concatMap (evToLocs bSh cSh) $ queryArc pat (Arc t t)
        where evToLocs a b (Event {context = Context xs, whole = wh}) = map (\x -> ((toLoc a b) x, wh)) xs
              -- assume an event doesn't span a line..
              toLoc blockSh colSh ((bx, by), (ex, _)) | by == 1 = (blockSh+by - 1, bx + colSh, ex + colSh)
                                                | otherwise = (blockSh+by - 1, bx, ex)

highlightLoop :: Buffer -> Stream -> Window -> MVar HighlightStates -> IO ()
highlightLoop buffer stream win patStatesMVar = do
                patStates <- readMVar patStatesMVar
                tempo <- readMVar $ sTempoMV stream
                t <- time
                let pats = Map.elems patStates
                    c = timeToCycles tempo t
                (marks,buffer') <- highlightPats c buffer win pats
                threadDelay 100000 --works pretty well, might have to be adjusted
                unhighlightMany marks win
                highlightLoop buffer' stream win patStatesMVar

highlightPats :: Rational -> Buffer -> Window -> [HighlightState] -> IO ([JSObject],Buffer)
highlightPats _ buffer _ [] = return ([], buffer)
highlightPats c buffer win ((HS pat bSh cSh True _):ps) = do
                                              let ls = locs c bSh cSh pat
                                              (marks,buffer') <- highlightPats c (buffer \\ ls) win ps
                                              return (marks, buffer')
highlightPats c buffer win ((HS pat bSh cSh False _):ps) = do
                                              let ls = locs c bSh cSh pat
                                              (marks,buffer') <- highlightMany buffer ls win
                                              (marks',buffer'') <- highlightPats c buffer' win ps
                                              return ((marks ++ marks'), buffer'')

highlightMany :: Buffer -> [((Int,Int,Int), Maybe Arc)] -> Window -> IO ([JSObject],Buffer)
highlightMany buffer [] _ = return ([],buffer)
highlightMany buffer (x@(i,_):xs) win = do
                                case elem x buffer of
                                  True -> highlightMany buffer xs win
                                  False -> do
                                      mark <- runUI win (highlight i)
                                      let buf = filter (\(j,_) -> j /= i) buffer -- so the buffer doesn't clog up
                                      (marks, buffer') <- highlightMany (x:buf) xs win
                                      return ((mark:marks),buffer')

unhighlightMany :: [JSObject] -> Window -> IO ()
unhighlightMany [] _ = return ()
unhighlightMany (x:xs) win = do
                    runUI win (unHighlight x)
                    unhighlightMany xs win

--flash on evaluation

highlightBlock :: Int -> Int -> String -> UI JSObject
highlightBlock lineStart lineEnd color = callFunction $ ffi "(controlEditor.markText({line: %1, ch: 0}, {line: %2, ch: 0}, {css: %3}))" lineStart lineEnd color

flashSuccess :: Int -> Int -> UI ()
flashSuccess lineStart lineEnd = do
                            mark <- highlightBlock (max (lineStart - 1) 0) (lineEnd + 1) "background-color: green"
                            liftIO $ threadDelay 100000
                            unHighlight mark
                            flushCallBuffer

flashError :: Int -> Int -> UI ()
flashError lineStart lineEnd = do
                            mark <- highlightBlock (max (lineStart - 1) 0) (lineEnd + 1) "background-color: red"
                            liftIO $ threadDelay 100000
                            unHighlight mark
                            flushCallBuffer
