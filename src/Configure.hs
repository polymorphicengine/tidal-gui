module Configure where

import Language.Haskell.Interpreter
import Sound.Tidal.Context(Schedule(Live))
import Sound.Tidal.Stream (Target(..))
import Data.List

libsU :: [String]
libsU = [
    "Sound.Tidal.Pattern"
  , "Sound.Tidal.Transition"
  , "Sound.Tidal.Context"
  , "Sound.Tidal.Simple"
  , "Control.Applicative"
  , "Data.Bifunctor"
  , "Data.Bits"
  , "Data.Bool"
  , "Data.Char"
  , "Data.Either"
  , "Data.Foldable"
  , "Data.Function"
  , "Data.Functor"
  , "Data.Int"
  , "Data.List"
  , "Data.Maybe"
  , "Data.Monoid"
  , "Data.Ord"
  , "Data.Ratio"
  , "Data.Semigroup"
  , "Data.String"
  , "Data.Traversable"
  , "Data.Tuple"
  , "Data.Typeable"
  , "Data.IORef"
  , "GHC.Float"
  , "GHC.Real"
  , "GHC.Err"
  , "System.IO"
  ]

libsU' :: [ModuleImport]
libsU' = [ModuleImport x NotQualified NoImportList | x <- libsU]

libs :: [ModuleImport]
libs = [ModuleImport "Data.Map" (NotQualified) (HidingList ["size"]), ModuleImport "Text.Parsec" NotQualified (HidingList ["Stream"])] ++ libsU'

exts :: [Extension]
exts = [OverloadedStrings]

listenPort,remotePort :: Int
listenPort = 6011
remotePort = 6012

remoteTarget :: Target
remoteTarget = Target {oName = "threepenny"
                      ,oAddress = "127.0.0.1"
                      ,oPort = remotePort
                      ,oBusPort = Nothing
                      ,oLatency = 0.1
                      ,oWindow = Nothing
                      ,oSchedule = Live
                      ,oHandshake = True
                      }

bootTidal' :: [String]
bootTidal' = [ "p = streamReplace tidal"
              ,"d1 pat = do p 1 $ pat |< orbit 0; return \"d1\""
              ,"d2 pat = do p 2 $ pat |< orbit 1; return \"d2\""
              ,"d3 pat = do p 3 $ pat |< orbit 2; return \"d3\""
              ,"d4 pat = do p 4 $ pat |< orbit 3; return \"d4\""
              ,"d5 pat = do p 5 $ pat |< orbit 4; return \"d5\""
              ,"d6 pat = do p 6 $ pat |< orbit 5; return \"d6\""
              ,"d7 pat = do p 7 $ pat |< orbit 6; return \"d7\""
              ,"d8 pat = do p 8 $ pat |< orbit 7; return \"d8\""
              ,"d9 pat = do p 9 $ pat |< orbit 8; return \"d9\""
              ,"hush = streamHush tidal"
              ,"panic = do hush; once $ sound \"superpanic\""
              ,"list = streamList tidal"
              -- ,"mute = streamMute tidal"
              --,"unmute = streamUnmute tidal :: Show a => a -> IO ()"
              ,"unmuteAll = streamUnmuteAll tidal"
              ,"unsoloAll = streamUnsoloAll tidal"
              --,"solo = streamSolo tidal :: Show a => a -> IO ()"
              --,"unsolo = streamUnsolo tidal :: Show a => a -> IO ()"
              ,"once = streamOnce tidal"
              ,"first = streamFirst tidal"
              ,"asap = once"
              ,"nudgeAll = streamNudgeAll tidal"
              ,"all = streamAll tidal"
              ,"resetCycles = streamResetCycles tidal"
              ,"setcps = asap . cps"
              ,"getcps = streamGetcps tidal"
              ,"getnow = streamGetnow tidal"
              ,"xfade i = transition tidal True (Sound.Tidal.Transition.xfadeIn 4) i"
              ,"xfadeIn i t = transition tidal True (Sound.Tidal.Transition.xfadeIn t) i"
              ,"histpan i t = transition tidal True (Sound.Tidal.Transition.histpan t) i"
              ,"wait i t = transition tidal True (Sound.Tidal.Transition.wait t) i"
              ,"waitT i f t = transition tidal True (Sound.Tidal.Transition.waitT f t) i"
              ,"jump i = transition tidal True (Sound.Tidal.Transition.jump) i"
              ,"jumpIn i t = transition tidal True (Sound.Tidal.Transition.jumpIn t) i"
              ,"jumpIn' i t = transition tidal True (Sound.Tidal.Transition.jumpIn' t) i"
              ,"jumpMod i t = transition tidal True (Sound.Tidal.Transition.jumpMod t) i"
              ,"mortal i lifespan release = transition tidal True (Sound.Tidal.Transition.mortal lifespan release) i"
              ,"interpolate i = transition tidal True (Sound.Tidal.Transition.interpolate) i"
              ,"interpolateIn i t = transition tidal True (Sound.Tidal.Transition.interpolateIn t) i"
              ,"clutch i = transition tidal True (Sound.Tidal.Transition.clutch) i"
              ,"clutchIn i t = transition tidal True (Sound.Tidal.Transition.clutchIn t) i"
              ,"anticipate i = transition tidal True (Sound.Tidal.Transition.anticipate) i"
              ,"anticipateIn i t = transition tidal True (Sound.Tidal.Transition.anticipateIn t) i"
              ,"forId i t = transition tidal False (Sound.Tidal.Transition.mortalOverlay t) i"
             ]

bootTidal :: String
bootTidal = "let \n" ++ (intercalate "\n" bootTidal')
