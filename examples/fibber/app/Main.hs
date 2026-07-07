module Main where

import Data.Foldable (for_)
import Debug.Trace (traceMarkerIO)
import System.Environment (getArgs)

main :: IO ()
main = do
  args <- getArgs
  for_ args $ \arg -> do
    traceMarkerIO $ "Starting fib " <> arg
    print $ fib (read arg)
    traceMarkerIO $ "Finished fib " <> arg

fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)
