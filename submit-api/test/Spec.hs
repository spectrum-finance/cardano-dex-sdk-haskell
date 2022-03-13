module Main(main) where

import Test.HUnit
import Models.Generator

import qualified System.Random as Random
import Control.Monad
import System.IO.Unsafe
import Test.RandomStrings


main :: IO ()
main = do
  let
    v = unsafePerformIO $ liftM (take 10 . Random.randomRs ('a','z')) Random.newStdGen
    v1 = unsafePerformIO $liftM (take 10 . Random.randomRs ('a','z')) Random.newStdGen
  print $ unsafePerformIO $ randomString randomChar8 10
  print $ unsafePerformIO $ randomString randomChar8 10
  print $ genTxCandidate