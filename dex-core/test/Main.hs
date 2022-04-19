module Main where

import qualified Data.Text.Encoding as E

import Test.Tasty
import Test.Tasty.HUnit

import Spec.Pool as PS

main :: IO ()
main = do
  defaultMain tests

tests = testGroup "DexCore" 
  [ PS.toFromLedgerPoolTests
  , PS.checkDeposit
  , PS.checkRedeem
  , PS.checkSwap
  , PS.initialLiquidityTests
  , PS.initPoolTests
  ]
