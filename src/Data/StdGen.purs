module Data.StdGen
  ( StdGen(..)
  , runStdGen
  , runStdGen'
  , StdSeed
  , mkStdSeed
  , randomStdSeed
  , nextStdSeed
  , nextInt53
  , nextNumber
  , splitStdSeed
  , nextMin
  , nextMax
  , Size
  ) where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Random (RANDOM, randomInt)
import Control.Monad.Gen.Class (class MonadGen)
import Control.Lazy (class Lazy)
import Control.Monad.Rec.Class (class MonadRec, Step(..), tailRec)

import Data.Int (toNumber)
import Data.Int.Bits ((.&.))
import Data.Tuple (Tuple(..), fst)
import Data.Int53 (Int53)
import Data.Int53 as Int53

type Size = Int

data StdSeed = StdSeed Int Int

instance showStdSeed :: Show StdSeed where
  show (StdSeed a b) = "(StdSeed " <> show a <> " " <> show b <> ")"

-- | The generator for random values of type `a`.
newtype StdGen a = StdGen (Tuple StdSeed Size -> a)

-- | Run a random generator.
runStdGen :: forall a. StdSeed -> Size -> StdGen a -> a
runStdGen seed size = runStdGen' (Tuple seed size)

runStdGen' :: forall a. Tuple StdSeed Size -> StdGen a -> a
runStdGen' a (StdGen gen) = gen a

-- | Create a new 'StdSeed' from a 32-bit integer.
mkStdSeed :: Int -> StdSeed
mkStdSeed s0 = StdSeed (s1 + 1) (s2 + 1)
  where
  -- The integer variables s1 and s2 ... must be initialized to values in the
  -- range [1, 2147483562] and [1, 2147483398] respectively.
  s  = s0 .&. top
  s1 = s `mod` nextMax
  q  = s `div` nextMax
  s2 = q `mod` 2147483398

-- create a new 'StdSeed' using `Random` effect
randomStdSeed :: forall eff. Eff (random :: RANDOM | eff) StdSeed
randomStdSeed = mkStdSeed <$> randomInt bottom top

nextMin :: Int
nextMin = 1

nextMax :: Int
nextMax = 2147483562

uniform :: StdGen Number
uniform = StdGen \(Tuple seed size) ->
  let Tuple x _ = nextStdSeed seed
  in toNumber x / toNumber 2147483397

-- | Returns the next pseudo-random number in the sequence, and a new StdSeed.
nextStdSeed :: StdSeed -> Tuple Int StdSeed
nextStdSeed (StdSeed s1 s2) = Tuple z' (StdSeed s1'' s2'')
  where
  k    = s1 `div` 53668
  s1'  = 40014 * (s1 - k * 53668) - k * 12211
  s1'' = if s1' < 0 then s1' + 2147483563 else s1'
  k'   = s2 `div` 52774
  s2'  = 40692 * (s2 - k' * 52774) - k' * 3791
  s2'' = if s2' < 0 then s2' + 2147483399 else s2'
  z    = s1'' - s2''
  z'   = if z < 1 then z + 2147483562 else z

nextInt53 :: Int53 -> Int53 -> StdSeed -> Tuple Int53 StdSeed
nextInt53 lo hi seed
  | lo > hi   = nextInt53 hi lo seed -- we are reversed the range here
  | otherwise =
      let
        b       = Int53.fromInt nextMax - Int53.fromInt nextMin + one
        q       = Int53.fromInt 1000
        k       = hi - lo + one
        magtgt  = k * q
        loop mag v0 seed0 =
          if mag >= magtgt
          then Tuple v0 seed0
          else
          	let
          	  Tuple x seed1 = nextStdSeed seed0
          	  v1 = v0 * b + (Int53.fromInt x - Int53.fromInt nextMin)
          	in loop (mag * b) v1 seed1
        Tuple v seedN = loop one zero seed
      in Tuple (lo + (v `mod` k)) seedN

nextNumber :: Number -> Number -> StdSeed -> Tuple Number StdSeed
nextNumber lo hi seed
  | lo > hi   = nextNumber hi lo seed
  | otherwise =
      let
        Tuple x seed' = nextStdSeed seed
        x'            = 2.0 * (0.5 * lo + (toNumber x / 2147483397.00) * (0.5 * hi - 0.5 * lo))
      in Tuple x' seed'

-- | Split `StdSeed` into two `StdSeed`
splitStdSeed :: StdSeed -> Tuple StdSeed StdSeed
splitStdSeed seed@(StdSeed s1 s2) = case nextStdSeed seed of
  Tuple _ (StdSeed t1 t2) ->
  	let
  	  ns1 = if s1 == 2147483562 then 1 else s1 + 1
  	  ns2 = if s2 == 1 then 2147483398 else s2 - 1
  	in Tuple (StdSeed ns1 t2) (StdSeed t1 ns2)

instance functorStdGen :: Functor StdGen where
  map f (StdGen gen) = StdGen (f <<< gen)

instance applyStdGen :: Apply StdGen where
  apply = ap

instance applicativeStdGen :: Applicative StdGen where
  pure a = StdGen \_ -> a

instance bindStdGen :: Bind StdGen where
  bind r k = StdGen \(Tuple seed size) ->
    let Tuple seed1 seed2 = splitStdSeed seed
    in runStdGen' (Tuple seed2 size) (k (runStdGen' (Tuple seed1 size) r))

instance monadStdGen :: Monad StdGen

instance monadGenStdGen :: MonadGen StdGen where
  chooseInt lo hi = StdGen \(Tuple seed _) ->
  	Int53.toInt $ fst $ nextInt53 (Int53.fromInt lo) (Int53.fromInt hi) seed
  chooseFloat lo hi = StdGen \(Tuple seed _) -> fst $ nextNumber lo hi seed
  chooseBool = (_ < 0.5) <$> uniform
  resize f g = StdGen \(Tuple seed size) -> runStdGen' (Tuple seed (f size)) g
  sized f = StdGen \(Tuple seed size) -> runStdGen' (Tuple seed size) (f size)

instance monadRecStdGen :: MonadRec StdGen where
  tailRecM k a0 = StdGen \(Tuple seed size) -> tailRec go ({ seed, size, a: a0 })
    where
    go { seed, size, a } =
      let Tuple seed1 seed2 = splitStdSeed seed
      in case runStdGen' (Tuple seed1 size) (k a) of
        Loop a1 -> Loop { seed: seed2, size, a: a1 }
        Done a2 -> Done a2

instance lazyStdGen :: Lazy (StdGen a) where
  defer f = StdGen \se -> runStdGen' se (f unit)
