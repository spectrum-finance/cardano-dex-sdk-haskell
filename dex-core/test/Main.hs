module Main where

import qualified Data.Text.Encoding as E

import Test.Tasty
import Test.Tasty.HUnit

import Spec.Pool as PS

main :: IO ()
main = defaultMain tests

tests = testGroup "DEX Core" [PS.initialLiquidityTests, PS.initPoolTests]
