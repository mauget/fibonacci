module Main where

import           Control.Monad.State.Lazy

fibArg :: Integer
fibArg = 1000

main :: IO ()
main =
  do
    putStr "fibonacci of "
    print fibArg
    print (fibonacci fibArg)

-- The rest is a pair of functions that carry out the fibonacci algorithm:

fibsState :: State (Integer, Integer, Integer) Integer
fibsState =
  get >>= \(x1, x2, n) ->
    if n == 0
      then return x1
      else put (x2, x1 + x2, n - 1) >> fibsState

fibonacci :: Integer -> Integer
fibonacci n = evalState fibsState (0, 1, n)
