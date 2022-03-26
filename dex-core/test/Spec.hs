module Main(main) where

import qualified Data.Text.Encoding as E

import Test.HUnit
import Eval

import Helper (successCase, failureCase, runTest)

main :: IO ()
main = do
  r <- runTestTT tests
  print r

tests = 
  TestList 
    []
