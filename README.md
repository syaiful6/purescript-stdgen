### Purescript Std-Random

This is port of Haskell's System Random, but with different API and has been recast in a form suitable for Purescript
ecosystem. This lib depend and implements ```MonadGen``` defined on [published on Pursuit](https://github.com/purescript/purescript-gen).

### Getting Started

The most basic use case is to choose an item in a collection:

```purescript
chooseCard :: NonEmpty f (StdGen a) -> StdGen a
chooseCard xs = oneOf xs
```

To run ```StdGen``` you need ```StdSeed```, you can manually create it using ```mkStdSeed``` or by using ```randomStdSeed``` that
use ```Javascript's random```. Usually you want to hold ```StdSeed``` after it created on mutable variable, and
update it after you use it.

```purescript
main = do
  seed <- randomStdSeed
  seedref <- newRef seed
  let Tuple seed1 seed2 = splitStdSeed seed
  log $ evalStdGen (chooseCard cards) (GenState seed1 10)
  -- modify the seed to use next time
  writeRef seedref seed2
  -- rest program
```

### Ceveats

- The lib only provides **pseudo-random** generator, it is not a source for cryptographic quality random bits.
- The random generator defined here is pure, meaning it will generate the same sequence each time they are called with the same Seed.
