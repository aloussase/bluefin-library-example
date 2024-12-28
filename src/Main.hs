{-# LANGUAGE KindSignatures, TypeOperators #-}
module Main where

import Data.Foldable (for_)
import Control.Monad (when)
import System.IO
import Data.List

import Bluefin.IO
import Bluefin.State
import Bluefin.Eff
import Bluefin.Exception
import Bluefin.Reader
import Bluefin.EarlyReturn

-- Bluefin: an effect library
-- 
-- * based on effectful but with value level effect handles
-- * well-typed implementation of the Handle pattern
-- * aims to make it easier to mix effects

-- * Examples

-- * Effectful fizzbuzz

(+=) :: (e :> es) => State [a] e -> a -> Eff es ()
st += s = modify st (s :)

fizzbuzz :: Int -> [String]
fizzbuzz upto = reverse $ runPureEff $ 
  evalState [] $ \ws -> do
    for_ [1 .. upto] $ \i -> do
      if i `mod` 3 == 0 && i `mod` 5 == 0 then
        ws += "FizzBuzz"
      else if i `mod` 3 == 0 then
        ws += "Fizz"
      else if i `mod` 5 == 0 then
        ws += "Buzz"
      else
        ws += show i
    get ws

-- * Injecting values into environment + exceptions

data Env = Env { configFilePath :: !String }
data Config = Config { host :: !String, port :: !Int }
  deriving Show

-- * Making compound effects
-- See: https://hackage.haskell.org/package/bluefin-0.0.14.0/docs/Bluefin-Compound.html
data ReadConfig e1 e2 e3 = ReadConfig (Reader Env e1) (Exception String e2) (IOE e3)

readConfig :: (e1 :> es, e2 :> es, e3 :> es) => ReadConfig e1 e2 e3 -> Eff es Config
readConfig (ReadConfig r ex io) = do
  filename <- asks r configFilePath
  contents <- effIO io $ readFile' filename
  case words contents of
    [host', port'] -> return $ Config host' (read port')
    _ -> throw ex "Could not parse config file"

runReadConfig :: IO (Either String Config)
runReadConfig =
  runEff $ \io ->
    try $ \ex -> 
      runReader (Env "config.txt") $ \env ->
        readConfig (ReadConfig env ex io)

-- Early return

isUnique :: (Ord a, Eq a) => [a] -> Bool
isUnique xs = runPureEff $ withEarlyReturn $ \e -> do
  let xs' = sort xs
  for_ [ 1 .. length xs' - 1 ] $ \i -> do
    when (xs' !! i == xs' !! (i - 1)) $
      returnEarly e False
  return True

-- * Other things not covered:
--
-- - Coroutines
-- - Jumps
-- - Pipes
-- - Stream
-- - Writer

main :: IO ()
main = undefined
