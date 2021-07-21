module Configure where

import Language.Haskell.Interpreter
import Sound.Tidal.Context(Schedule(Live))
import Sound.Tidal.Stream (Target(..))

libsU = [
    "Sound.Tidal.Pattern"
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
  , "GHC.Float"
  , "GHC.Real"
  , "Text.Parsec"
  ]

libsU' = [ModuleImport x NotQualified NoImportList | x <- libsU]

libs = [ModuleImport "Data.Map" (NotQualified) (HidingList ["size"])] ++ libsU'

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
