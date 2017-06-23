module Test.Data.StdSeed where

import Prelude

import Control.Monad.Gen as G
import Control.Monad.Rec.Class (Step(..), tailRecM)
import Jack as J
import Jack.Random (Random, chooseInt) as JR

import Data.Int (toNumber)
import Data.StdGen (StdSeed, mkStdSeed, runStdGen')
import Data.Tuple (Tuple(..))

genStdSeed :: J.Gen StdSeed
genStdSeed = mkStdSeed <$> J.boundedInt

pertubSeed :: J.Gen Int
pertubSeed = J.chooseInt 1 2147483562

genStdGenParam :: J.Gen (Tuple StdSeed Int)
genStdGenParam = Tuple <$> genStdSeed <*> pertubSeed

genChooseInt :: J.Gen (Tuple (Tuple StdSeed Int) Int)
genChooseInt = Tuple <$> genStdGenParam <*> J.sized sizeStdGen
  where
  sizeStdGen i
   | i == 0    = J.chooseInt 0 (i + 1)
   | i < 0     = J.chooseInt 0 (negate i)
   | otherwise = J.chooseInt 0 i

genChooseFloat :: J.Gen (Tuple (Tuple StdSeed Int) (Tuple Number Int))
genChooseFloat = Tuple <$> genStdGenParam <*> (Tuple <$> J.mkGen_ loopMin <*> pertubSeed)
  where
  loopMin = do
    let loop x = if x < zero then Loop <$> randNumber else pure (Done x)
    x0 <- randNumber
    tailRecM loop x0

randNumber :: JR.Random Number
randNumber = (\i -> toNumber i / 2147483647.0) <$> JR.chooseInt 1 2147483562

prop_chooseInt :: J.Property
prop_chooseInt = J.forAll genChooseInt \(Tuple p i) ->
  let res = runStdGen' p (G.chooseInt zero i)
  in J.property (res >= zero && res <= i)

prop_chooseFloat :: J.Property
prop_chooseFloat = J.forAll genChooseFloat \(Tuple p (Tuple n i)) ->
  let
    num = n + toNumber i
    res = runStdGen' p (G.chooseFloat zero num)
  in J.property (res >= zero && res <= num)
