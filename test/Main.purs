module Test.Main where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log, logShow)
import Control.Monad.Gen.Class (chooseInt, chooseFloat)
import Control.Monad.Eff.Random (RANDOM)

import Data.Array.Partial (head)
import Data.Foldable (sum)

import Partial.Unsafe (unsafePartial)

import System.Random (vectorOf, randomSample', shuffle)

main :: forall e. Eff (console :: CONSOLE, random :: RANDOM | e) Unit
main = do
  log "Try with some little Gens first"
  logShow =<< go 10
  logShow =<< go 100
  logShow =<< go 1000
  logShow =<< go 10000

  log "Testing stack safety of Gen"
  logShow =<< go 20000
  logShow =<< go 100000

  log "Testing shuffle"
  let xs = [12, 32, 34, 43, 21, 22, 33, 45]
  logShow =<< randomSample' 10 (shuffle xs)
  logShow =<< randomSample' 10 (chooseFloat 0.00 1.00)

  where
  go n = map (sum <<< unsafeHead) $ randomSample' 1 (vectorOf n (chooseInt (-1000000) 1000000))

  unsafeHead :: forall x. Array x -> x
  unsafeHead xs = unsafePartial (head xs)
