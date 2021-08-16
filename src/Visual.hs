module Visual where

import Sound.Tidal.Context as T hiding (solo,(#))
import Data.List (groupBy)
import Data.Map as Map (Map,toList,lookup, insert)

import Control.Monad (void)
import Control.Concurrent (threadDelay)
import Control.Concurrent.MVar (MVar, readMVar, takeMVar, putMVar)
import Sound.OSC.FD (time)
import Sound.Tidal.Tempo (timeToCycles)

import Numeric (showFFloat)

import System.Random

import Graphics.UI.Threepenny as UI hiding (map)
import qualified Graphics.UI.Threepenny.SVG  as SVG

-- Transform a pattern to a html SVG element that can be displayed

data VisEvent a = VAt {vStart :: Time
                      ,vEnd :: Time
                      ,vVal :: a
                      }

type VisPat a = [VisEvent a]

--map sample names to colors
--might change first argument from Value to String, so users can write custom color mappings easyily
type ColorMap = Map String String

patToVis :: Time -> Pattern a -> VisPat a
patToVis t pat = map evToVis evs
          where evs = filter eventHasOnset $ queryArc pat (Arc t (t+1))

evToVis :: T.Event a -> VisEvent a
evToVis ev = VAt x y (T.value ev)
          where (Arc x y) = part ev

evToSVGControl :: Int -> Time -> MVar ColorMap -> VisEvent ValueMap  -> UI Element
evToSVGControl i t cMapMV (VAt st en valMap) = case Map.lookup "s" valMap of
                                              Just (VS samp) -> do
                                                          cM <- liftIO $ takeMVar cMapMV
                                                          case Map.lookup samp cM of
                                                            Just col -> do
                                                                      _ <- liftIO $ putMVar cMapMV cM
                                                                      SVG.rect
                                                                            # set SVG.width wi
                                                                            # set SVG.x stPos
                                                                            # set SVG.y yPos
                                                                            # set SVG.height (show heightScale)
                                                                            # set SVG.stroke "black"
                                                                            # set SVG.stroke_width "2"
                                                                            # set SVG.fill col
                                                            Nothing   -> do
                                                                      c <- liftIO $ getRandomColor
                                                                      _ <- liftIO $ putMVar cMapMV (Map.insert samp c cM)
                                                                      SVG.rect
                                                                            # set SVG.width wi
                                                                            # set SVG.x stPos
                                                                            # set SVG.y yPos
                                                                            # set SVG.height (show heightScale)
                                                                            # set SVG.stroke "black"
                                                                            # set SVG.stroke_width "2"
                                                                            # set SVG.fill c
                                              _ -> SVG.rect
                      where wi = show $ (fromRational (en - st)*widthScale :: Double)
                            stPos = show $ (fromRational (st - t)*widthScale :: Double)
                            yPos = show $ i*heightScale
                            widthScale = 400
                            heightScale = 25

getRandomColor :: IO String
getRandomColor = do
            g <- newStdGen
            colors <- return $ take 3 (randomRs (0, 255) g :: [Int])
            return $ "rgb(" ++ show (colors!!0) ++ "," ++ show (colors!!1) ++ "," ++ show (colors!!2) ++ ")"

svgHead :: UI Element
svgHead = SVG.svg
            # set SVG.x "50%" -- position relative to window
            # set SVG.y "60%"
            # set style [("overflow","visible")]

visPatLine :: Integer -> Time -> MVar ColorMap -> VisPat ValueMap -> [UI Element]
visPatLine i t cMV evs = map (evToSVGControlArc i t cMV) evs

visPatRec :: Time -> MVar ColorMap -> [VisPat ValueMap] -> [UI Element]
visPatRec t cMV vs = concatMap (\(i,e) -> visPatLine i t cMV e) (zip [5..] vs)

patToSVGChan :: Time -> MVar ColorMap  -> [VisPat ValueMap] -> UI Element
patToSVGChan t cMV ps = svgHead #+ visPatRec t cMV ps


--groups by the value, so for a controlpattern groups by the valuemap as a whole
groupByVals :: Eq a => VisPat a -> [VisPat a]
groupByVals = groupBy (\x y -> vVal x == vVal y)

--groups by channels (standard)
groupByChan :: Time -> [ControlPattern] -> [VisPat ValueMap]
groupByChan t = map (patToVis t)

--get patterns playing and transform them to svg elements
makeSVG :: Stream -> MVar ColorMap -> UI Element
makeSVG str cMV = do
            tempo <- liftIO $ readMVar $ sTempoMV str
            pMap <- liftIO $ readMVar $ sPMapMV str
            t <- time
            let c = timeToCycles tempo t
                pats = map pattern $ filter (\x -> not (mute x)) (map snd $ toList pMap)
            patToSVGChan c cMV (groupByChan c pats)

-- doesn't perform good with many elements
visualizeStreamLoop :: Window -> Element -> Stream -> MVar ColorMap -> IO ()
visualizeStreamLoop win svg str cMapMV = do
                        void $ runUI win $ do
                                    d <- SVG.svg
                                            # set SVG.width "97vw"
                                            # set SVG.height "70vh"
                                            #+ [makeSVG str cMapMV]
                                    element svg # set UI.children [d]
                        threadDelay 100000
                        visualizeStreamLoop win svg str cMapMV

--------- arcs

makeArcD :: String -> String -> ((String, String), (String, String)) -> String
makeArcD radius larcflag ((startX, startY), (endX, endY)) = "M " ++ startX ++ " " ++ startY ++
                                                           " A " ++ radius ++ " " ++ radius ++ " 0 " ++ larcflag ++ " 1 " ++ endX ++ " " ++ endY

angle :: Float -> Float -> Float -> Float
angle intervalStart intervalEnd inp = 2*pi*(inp - intervalStart) / (intervalEnd - intervalStart)

arcPosition :: Float -> Float -> Float -> Float -> Float -> ((String,String),(String,String))
arcPosition intervalStart intervalEnd startPos endPos r = ((startPosArcX, startPosArcY), (endPosArcX, endPosArcY))
                                                        where startPosArcX = (showFFloat (Just 2) $ r*(cos $ startAngle )) ""
                                                              startPosArcY = (showFFloat (Just 2) $ r*(sin $ startAngle ) )  ""
                                                              endPosArcX = (showFFloat (Just 2) $ r*(cos $ endAngle ))  ""
                                                              endPosArcY = (showFFloat (Just 2) $ r*(sin $ endAngle ))  ""
                                                              startAngle = angle intervalStart intervalEnd startPos
                                                              endAngle = angle intervalStart intervalEnd endPos


makeArc :: Float -> Float -> Float -> Float -> Float -> String
makeArc intervalStart intervalEnd startPos endPos r | endPos - startPos <= intervalEnd / 2 = makeArcD (show r) "0" $ arcPosition intervalStart intervalEnd startPos endPos r  --short way
                                                    | otherwise = makeArcD (show r) "1" $ arcPosition intervalStart intervalEnd startPos endPos r                             --long way


-- each event corresponds to an arc
evToSVGControlArc :: Integer -> Time -> MVar ColorMap -> VisEvent ValueMap  -> UI Element
evToSVGControlArc i t cMapMV (VAt st en valMap) =
                                    case Map.lookup "s" valMap of
                                             Just (VS samp) -> do
                                                         cM <- liftIO $ takeMVar cMapMV
                                                         case Map.lookup samp cM of
                                                           Just col -> do
                                                                     _ <- liftIO $ putMVar cMapMV cM
                                                                     SVG.path
                                                                           # set SVG.d arcD
                                                                           # set SVG.stroke col
                                                                           # set SVG.stroke_width "7"
                                                                           # set SVG.fill "none"
                                                                           # set SVG.stroke_linecap "round"
                                                                           # set style [("filter",brightness)]
                                                           Nothing   -> do
                                                                     c <- liftIO $ getRandomColor
                                                                     _ <- liftIO $ putMVar cMapMV (Map.insert samp c cM)
                                                                     SVG.path
                                                                           # set SVG.d arcD
                                                                           # set SVG.stroke c
                                                                           # set SVG.stroke_width "7"
                                                                           # set SVG.fill "none"
                                                                           # set SVG.stroke_linecap "round"
                                                                           # set style [("filter",brightness)]
                                             _ -> SVG.path
                                   where wi = fromRational (en - st)*widthScale :: Float
                                         stPos = fromRational (st - t)*widthScale :: Float
                                         endPos = stPos + wi
                                         r = fromInteger (i*heightScale) ::Float
                                         arcD = makeArc 0 widthScale stPos endPos r
                                         widthScale = 400
                                         heightScale = 7
                                         brightness = if (abs (widthScale - stPos) <= 50) then "brightness(2)" else "brightness(1)"
