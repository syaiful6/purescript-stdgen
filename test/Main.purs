module Test.Main where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Eff.Random (RANDOM)

import Jack.Runner (jackMain)

main :: forall e. Eff ("random" :: RANDOM, "console" :: CONSOLE | e) Unit
main = jackMain
  [ "Test.Data.StdSeed"
  ]
