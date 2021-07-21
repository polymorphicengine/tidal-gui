module Configure where

import Language.Haskell.Interpreter
import Sound.Tidal.Context(Schedule(Live))
import Sound.Tidal.Stream (Target(..))
import Data.List

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

libsU' = [ModuleImport x NotQualified NoImportList | x <- libsU]

libs = [ModuleImport "Data.Map" (NotQualified) (HidingList ["size"]), ModuleImport "Text.Parsec" NotQualified (HidingList ["Stream"])] ++ libsU'

exts = [OverloadedStrings, NoImplicitPrelude]


listenPort = 6011
remotePort = 6012

remoteTarget = Target {oName = "threepenny"
                      ,oAddress = "127.0.0.1"
                      ,oPort = remotePort
                      ,oBusPort = Nothing
                      ,oLatency = 0.1
                      ,oWindow = Nothing
                      ,oSchedule = Live
                      ,oHandshake = True
                      }

bootTidal' = [ "p = streamReplace tidal :: String -> ControlPattern -> IO ()"
              ,"hush = streamHush tidal"
              ,"panic = do hush; once $ sound \"superpanic\""
              ,"list = streamList tidal"
              --,"mute = streamMute tidal"
              --,"unmute = streamUnmute tidal"
              ,"unmuteAll = streamUnmuteAll tidal"
              ,"unsoloAll = streamUnsoloAll tidal"
              --,"solo = streamSolo tidal"
              --,"unsolo = streamUnsolo tidal"
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

bootTidal = "let \n" ++ (intercalate "\n" bootTidal')
