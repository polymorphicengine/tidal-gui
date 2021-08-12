module Visual where

import Sound.Tidal.Context as T hiding (mute,solo,(#))
import Data.List (groupBy)

import Graphics.UI.Threepenny as UI hiding (map)
import qualified Graphics.UI.Threepenny.SVG  as SVG

-- Transform a pattern to a html SVG element that can be displayed

data VisEvent a = VAt {vStart :: Time
                      ,vEnd :: Time
                      ,vVal :: a
                      }

type VisPat a = [VisEvent a]

patToVis :: Pattern a -> VisPat a
patToVis pat = map evToVis evs
          where evs = queryArc pat (Arc 0 1)

evToVis :: T.Event a -> VisEvent a
evToVis ev = VAt x y (T.value ev)
          where (Arc x y) = part ev

evToSVG :: Int -> VisEvent a -> UI Element
evToSVG i (VAt st en _) = SVG.rect
                            # set SVG.width wi
                            # set SVG.x stPos
                            # set SVG.y yPos
                            # set SVG.height "25"
                            # set SVG.stroke "green"
                            # set SVG.stroke_width "2"
                            # set SVG.fill "yellow"
                      where wi = show $ (fromRational (en - st)*400 :: Double)
                            stPos = show $ (fromRational st*400 :: Double)
                            yPos = show $ i*25

visPatToSVG :: Eq a => VisPat a -> UI Element
visPatToSVG evs = SVG.svg
        # set SVG.width "100vw"
        # set SVG.height "200"
        #+ elems
        where elems = (visPatRec . groupByVals) evs

patToSVG :: Eq a => Pattern a -> UI Element
patToSVG = visPatToSVG . patToVis

groupByVals :: Eq a => VisPat a -> [VisPat a]
groupByVals = groupBy (\x y -> vVal x == vVal y)

visPatLine :: Int -> VisPat a -> [UI Element]
visPatLine i evs = map (evToSVG i) evs

visPatRec :: [VisPat a] -> [UI Element]
visPatRec vs = concatMap (\(i,e) -> visPatLine i e) (zip [0..] vs)
