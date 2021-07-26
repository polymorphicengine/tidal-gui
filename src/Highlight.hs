module Highlight where

import Sound.Tidal.Context
import Sound.Tidal.Tempo  (timeToCycles)
import Sound.OSC.FD (time)

import Graphics.UI.Threepenny.Core as C hiding (text)
import Foreign.JavaScript (JSObject)

import Control.Concurrent (forkIO,threadDelay)
import Control.Concurrent.MVar  (newEmptyMVar, readMVar, tryTakeMVar, takeMVar, MVar, putMVar)

import Data.Map as Map  (Map,elems)
import Data.List  ((\\))

--each event can be identified through it's whole, so it's drawn only when it first appears
type Buffer = [((Int,Int,Int), Maybe Arc)]

data PatternState = PS {sPat :: ControlPattern
                       ,bShift :: Int
                       ,cShift :: Int
                       ,muted :: Bool
                       ,solo :: Bool
                       } deriving Show

type PatternStates = Map Int PatternState

highlight :: (Int, Int, Int) -> UI JSObject
highlight (line, start, end) = callFunction $ ffi "(controlEditor.markText({line: %1, ch: %2}, {line: %1, ch: %3}, {css: \"background-color: red\"}))" line start end

-- unHighlight :: JSObject -> UI ()
-- unHighlight mark = runFunction $ ffi "if (typeof %1 !== 'undefined'){%1.clear()};" mark

unHighlight :: JSObject -> UI ()
unHighlight mark = runFunction $ ffi "%1.clear();" mark

locs :: Rational -> Int -> Int -> ControlPattern -> [((Int,Int,Int), Maybe Arc)]
locs t bShift cShift pat = concatMap (evToLocs bShift cShift) $ queryArc pat (Arc t t)
        where evToLocs bShift cShift (Event {context = Context xs, whole = wh}) = map (\x -> ((toLoc bShift cShift) x, wh)) xs
              -- assume an event doesn't span a line..
              toLoc bShift cShift ((bx, by), (ex, _)) | by == 1 = (bShift+by - 1, bx + cShift, ex + cShift)
                                                      | otherwise = (bShift+by - 1, bx, ex)
              locsWithArc ls = zip ls (map whole $ queryArc pat (Arc t t))

highlightLoop :: Buffer -> Stream -> Window -> MVar PatternStates -> IO ()
highlightLoop buffer stream win patStatesMVar = do
                patStates <- readMVar patStatesMVar
                tempo <- readMVar $ sTempoMV stream
                t <- time
                let pats = Map.elems patStates
                    c = timeToCycles tempo t
                (marks,buffer') <- highlightPats c buffer win pats
                threadDelay 100000 --works pretty well, might have to be adjusted
                unhighlightMany marks win
                runUI win flushCallBuffer
                highlightLoop buffer' stream win patStatesMVar

highlightPats :: Rational -> Buffer -> Window -> [PatternState] -> IO ([JSObject],Buffer)
highlightPats c buffer win [] = return ([], buffer)
highlightPats c buffer win ((PS pat bShift cShift True _):ps) = do
                                              let ls = locs c bShift cShift pat
                                              (marks,buffer') <- highlightPats c (buffer \\ ls) win ps
                                              return (marks, buffer')
highlightPats c buffer win ((PS pat bShift cShift False _):ps) = do
                                              let ls = locs c bShift cShift pat
                                              (marks,buffer') <- highlightMany buffer ls win
                                              (marks',buffer'') <- highlightPats c buffer' win ps
                                              return ((marks ++ marks'), buffer'')

highlightMany :: Buffer -> [((Int,Int,Int), Maybe Arc)] -> Window -> IO ([JSObject],Buffer)
highlightMany buffer [] win = return ([],buffer)
highlightMany buffer (x@(i,a):xs) win = do
                                case elem x buffer of
                                  True -> highlightMany buffer xs win
                                  False -> do
                                      mark <- runUI win (highlight i)
                                      let buf = filter (\(j,_) -> j /= i) buffer -- so the buffer doesn't clog up
                                      (marks, buffer') <- highlightMany (x:buf) xs win
                                      return ((mark:marks),buffer')

unhighlightMany :: [JSObject] -> Window -> IO ()
unhighlightMany [] win = return ()
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
