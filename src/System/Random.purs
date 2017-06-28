module System.Random
  ( StdGen(..)
  , runStdGen
  , evalStdGen
  , StdSeed
  , mkStdSeed
  , randomStdSeed
  , nextStdSeed
  , splitStdSeed
  , nextMin
  , nextMax
  , GenState(..)
  , Size
  -- Combinator
  , stateful
  , variant
  , replicateMRec
  , listOf
  , vectorOf
  , shuffle
  , randomSample'
  , randomSample
  ) where

import Prelude

import Control.Alt (class Alt)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Random (RANDOM, randomInt)
import Control.Monad.Gen.Class (class MonadGen, chooseInt)
import Control.Lazy (class Lazy)
import Control.Monad.Rec.Class (class MonadRec, Step(..), tailRecM)
import Control.Monad.State (State, runState, evalState)
import Control.Monad.State.Class (state)

import Data.Array (length, zip, sortBy)
import Data.Int (toNumber, floor)
import Data.Int.Bits ((.&.))
import Data.List (List(..), toUnfoldable)
import Data.Tuple (Tuple(..), snd, fst)

import Math ((%))


type Size = Int

-- | Splittable random number generator.
data StdSeed = StdSeed Int Int

instance showStdSeed :: Show StdSeed where
  show (StdSeed a b) = "(StdSeed " <> show a <> " " <> show b <> ")"

instance eqStdSeed :: Eq StdSeed where
  eq (StdSeed s1 s2) (StdSeed t1 t2) = s1 == t1 && s2 == t2

-- | Create a new 'StdSeed' from a 32-bit integer.
mkStdSeed :: Int -> StdSeed
mkStdSeed s0 = StdSeed (s1 + 1) (s2 + 1)
  where
  -- The integer variables s1 and s2 ... must be initialized to values in the
  -- range [1, 2147483562] and [1, 2147483398] respectively.
  s  = s0 .&. top
  s1 = s `mod` 2147483562
  q  = s `div` 2147483562
  s2 = q `mod` 2147483398

-- | create a new 'StdSeed' using `Random` effect
randomStdSeed :: forall eff. Eff (random :: RANDOM | eff) StdSeed
randomStdSeed = mkStdSeed <$> randomInt bottom top

-- | The smallest possible value returned from 'nextStdSeed'.
nextMin :: Int
nextMin = 1

-- | The largest possible value returned from 'nextStdSeed'.
nextMax :: Int
nextMax = 2147483562

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

-- | Split `StdSeed` into two `StdSeed`
splitStdSeed :: StdSeed -> Tuple StdSeed StdSeed
splitStdSeed seed@(StdSeed s1 s2) = case nextStdSeed seed of
  Tuple _ (StdSeed t1 t2) ->
    let
      ns1 = if s1 == 2147483562 then 1 else s1 + 1
      ns2 = if s2 == 1 then 2147483398 else s2 - 1
    in Tuple (StdSeed ns1 t2) (StdSeed t1 ns2)

--------------------------------------------------------------------------------
-- Random Generator ------------------------------------------------------------
--------------------------------------------------------------------------------

-- The Generator State parameterized with Seed and Size. The meaning of Size here
-- depend on depends on the particular generator used.
data GenState = GenState StdSeed Size

instance showGenState :: Show GenState where
  show (GenState seed size) = "(GenState " <> show seed <> " " <> show size <> " )"

-- | The generator for random values of type `a`.
newtype StdGen a = StdGen (State GenState a)

-- unwrap StdGen
unStdGen :: forall a. StdGen a -> State GenState a
unStdGen (StdGen a) = a

-- | Run the random generator
runStdGen :: forall a. StdGen a -> GenState -> Tuple a GenState
runStdGen = runState <<< unStdGen

-- | Run a random generator, keeping only the randomly-generated result
evalStdGen :: forall a. StdGen a -> GenState -> a
evalStdGen = evalState <<< unStdGen

derive newtype instance functorGen :: Functor StdGen
derive newtype instance applyGen :: Apply StdGen
derive newtype instance applicativeGen :: Applicative StdGen
derive newtype instance bindGen :: Bind StdGen
derive newtype instance monadGen :: Monad StdGen
derive newtype instance altGen :: Alt StdGen
derive newtype instance monadRecGen :: MonadRec StdGen
derive newtype instance lazyGen :: Lazy (StdGen a)

instance monadGenStdGen :: MonadGen StdGen where
  chooseInt a b = if a <= b then chooseInt' a b else chooseInt' b a
  chooseFloat = choose
  chooseBool = (_ < 0.5) <$> uniform
  resize f g = StdGen $ state resized
    where
    resized = \(GenState seed size) -> runStdGen g (GenState seed (f size))
  sized f = StdGen $ state sized'
    where
    sized' = \(GenState seed size) -> runStdGen (f size) (GenState seed size)

--------------------------------------------------------------------------------
-- Combinator ------------------------------------------------------------------
--------------------------------------------------------------------------------

-- | Create a random generator which uses the generator state explicitly
stateful :: forall a. (GenState -> StdGen a) -> StdGen a
stateful f = StdGen (state \s -> runStdGen (f s) s)

-- | Modify a random generator by setting a new random seed
variant :: forall a. StdSeed -> StdGen a -> StdGen a
variant n g = StdGen (state \(GenState seed size) -> runStdGen g (GenState n size))

replicateMRec :: forall m a. MonadRec m => Int -> m a -> m (List a)
replicateMRec k _ | k <= 0 = pure Nil
replicateMRec k gen = tailRecM go (Tuple Nil k)
  where
  go :: (Tuple (List a) Int) -> m (Step (Tuple (List a) Int) (List a))
  go (Tuple acc 0) = pure $ Done acc
  go (Tuple acc n) = map (\x -> Loop (Tuple (Cons x acc) (n - 1))) gen

-- | Create a random generator which generates a list of random values of the specified size.
listOf :: forall a. Int -> StdGen a -> StdGen (List a)
listOf = replicateMRec

-- | Create a random generator which generates a vector of random values of a specified size.
vectorOf :: forall a. Int -> StdGen a -> StdGen (Array a)
vectorOf k g = toUnfoldable <$> listOf k g

-- | Generate a random permutation of the given array
shuffle :: forall a. Array a -> StdGen (Array a)
shuffle xs = do
  ns <- vectorOf (length xs) (chooseInt 0 top)
  pure (map snd (sortBy (comparing fst) (zip ns xs)))

-- | Sample a random generator
sample :: forall a. StdSeed -> Size -> StdGen a -> Array a
sample seed sz g = evalStdGen (vectorOf sz g) (GenState seed sz)

-- | Sample a random generator, using a randomly generated seed
randomSample' :: forall r a. Size -> StdGen a -> Eff (random :: RANDOM | r) (Array a)
randomSample' size gen = do
  seed <- randomStdSeed
  pure $ sample seed size gen

-- | Get a random sample of 10 values
randomSample :: forall r a. StdGen a -> Eff (random :: RANDOM | r) (Array a)
randomSample = randomSample' 10

-- | Create a random generator which samples a range of `Number`s i
-- | with uniform probability.
choose :: Number -> Number -> StdGen Number
choose a b = (*) (max' - min') >>> (+) min' <$> uniform where
  min' = min a b
  max' = max a b

chooseInt' :: Int -> Int -> StdGen Int
chooseInt' a b = floor <<< clamp <$> choose32BitPosNumber
  where
  choose32BitPosNumber :: StdGen Number
  choose32BitPosNumber =
    (+) <$> choose31BitPosNumber <*> (((*) 2.0) <$> choose31BitPosNumber)

  choose31BitPosNumber :: StdGen Number
  choose31BitPosNumber = toNumber <$> genStep

  clamp :: Number -> Number
  clamp x = numA + (x % (numB - numA + one))

  numA = toNumber a
  numB = toNumber b

-- | Simple random generator that simply use `nextStdSeed` for it return value
genStep :: StdGen Int
genStep = StdGen $ state \(GenState seed size) ->
  map (flip GenState size) (nextStdSeed seed)

-- | A random generator which approximates a uniform random variable on `[0, 1]`
uniform :: StdGen Number
uniform = (\n -> toNumber n / toNumber 2147483563) <$> genStep
