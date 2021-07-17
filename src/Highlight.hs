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

type Buffer = [((Int,Int,Int), Maybe Arc)]

data PatternState = PS {sPat :: ControlPattern
                       ,blockLine :: Int
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

locs :: Rational -> Int -> ControlPattern -> [((Int,Int,Int), Maybe Arc)]
locs t bShift pat = concatMap (evToLocs bShift) $ queryArc pat (Arc t t)
        where evToLocs bShift (Event {context = Context xs, whole = wh}) = map (\x -> ((toLoc bShift) x, wh)) xs
              -- assume an event doesn't span a line..
              toLoc bShift ((bx, by), (ex, _)) | by == 1 = (bShift+by - 1, bx + 4, ex + 4)
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
highlightPats c buffer win ((PS pat shift True _):ps) = do
                                              let ls = locs c shift pat
                                              (marks,buffer') <- highlightPats c (buffer \\ ls) win ps
                                              return (marks, buffer')
highlightPats c buffer win ((PS pat shift False _):ps) = do
                                              let ls = locs c shift pat
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
