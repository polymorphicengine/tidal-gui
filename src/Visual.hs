module Visual where

import Sound.Tidal.Context as T hiding (mute,solo,(#))
import Data.List (groupBy)
import Data.Map (toList)

import Control.Monad (void)
import Control.Concurrent (threadDelay)
import Control.Concurrent.MVar (readMVar)
import Sound.OSC.FD (time)
import Sound.Tidal.Tempo (timeToCycles)

import Graphics.UI.Threepenny as UI hiding (map)
import qualified Graphics.UI.Threepenny.SVG  as SVG

-- Transform a pattern to a html SVG element that can be displayed

data VisEvent a = VAt {vStart :: Time
                      ,vEnd :: Time
                      ,vVal :: a
                      }

type VisPat a = [VisEvent a]

patToVis :: Time -> Pattern a -> VisPat a
patToVis t pat = map evToVis evs
          where evs = filter eventHasOnset $ queryArc pat (Arc t (t+1))

evToVis :: T.Event a -> VisEvent a
evToVis ev = VAt x y (T.value ev)
          where (Arc x y) = part ev

evToSVG :: Int -> Time -> VisEvent a -> UI Element
evToSVG i t (VAt st en _) = SVG.rect
                                # set SVG.width wi
                                # set SVG.x stPos
                                # set SVG.y yPos
                                # set SVG.height "25"
                                # set SVG.stroke "green"
                                # set SVG.stroke_width "2"
                                # set SVG.fill "yellow"
                      where wi = show $ (fromRational (en - st)*400 :: Double)
                            stPos = show $ (fromRational (st - t)*400 :: Double)
                            yPos = show $ i*25


svgHead :: UI Element
svgHead = SVG.svg
        # set SVG.width "100vw"
        # set SVG.height "200"

visPatToSVG :: Eq a => Time -> VisPat a -> [UI Element]
visPatToSVG t = (visPatRec t) . groupByVals

patToSVG :: Eq a => Time -> Pattern a -> UI Element
patToSVG t pat = svgHead #+ els
            where els = ((visPatToSVG t) . (patToVis t)) pat

groupByVals :: Eq a => VisPat a -> [VisPat a]
groupByVals = groupBy (\x y -> vVal x == vVal y)

visPatLine :: Int -> Time -> VisPat a -> [UI Element]
visPatLine i t evs = map (evToSVG i t) evs

visPatRec :: Time -> [VisPat a] -> [UI Element]
visPatRec t vs = concatMap (\(i,e) -> visPatLine i t e) (zip [0..] vs)


visualizeStream :: Stream -> UI Element
visualizeStream str = do
            tempo <- liftIO $ readMVar $ sTempoMV str
            pMap <- liftIO $ readMVar $ sPMapMV str
            t <- time
            let c = timeToCycles tempo t
                pat = stack $ map (pattern . snd) $ toList pMap
            patToSVG c pat

visualizeStreamLoop :: Window -> Element -> Stream -> IO ()
visualizeStreamLoop win svg str = do
                        void $ runUI win $ do d <- visualizeStream str ; element svg # set UI.children [d]
                        threadDelay 100000
                        visualizeStreamLoop win svg str
