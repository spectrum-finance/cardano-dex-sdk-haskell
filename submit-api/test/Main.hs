module Main where

import qualified Data.Text.Encoding as E

import Test.Tasty
import Test.Tasty.HUnit

import Spec.Transaction
import System.Exit (exitFailure)
import Control.Monad (unless)

main :: IO ()
main = defaultMain tests

tests = testGroup "Submit API Tests"
  [ buildTxBodyTests
  , buildTxBodyContentTests 
  , buildBalancedTxTests 
  ]
