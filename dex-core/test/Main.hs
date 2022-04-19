module Main where

import qualified Data.Text.Encoding as E

import Test.Tasty
import Test.Tasty.HUnit

import Spec.Pool as PS

main :: IO ()
main = do
  print PS.at
  defaultMain tests

tests = testGroup "DexCore" 
  [ PS.toFromLedgerPoolTests
  , PS.lqAmountTests
  , PS.applyDepositTests
  , PS.shareAmountTests
  ]
--[PS.initialLiquidityTests, PS.initPoolTests]
