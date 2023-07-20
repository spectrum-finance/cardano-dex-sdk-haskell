{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.Text.Encoding as E

import Test.Tasty
import Test.Tasty.HUnit

import Spec.Transaction
import System.Exit (exitFailure)
import Control.Monad (unless)
import CardanoTx.Address

main :: IO ()
main = do
  print $ show $ readShellyAddress "addr1x9cgs59t2hr5sphrv4gfuzxl323akly5z57qv07hq266evkqwx9ghwy6quk2fhu5g0ek8rth7z4zxr5ev975ph34q5fsq2amyd"
  print $ show $ readShellyAddress "addr1w9cgs59t2hr5sphrv4gfuzxl323akly5z57qv07hq266evsg37dfw"
  --defaultMain tests

tests = testGroup "SubmitApi"
  [ buildTxBodyTests
  , buildTxBodyContentTests 
  , buildBalancedTxTests 
  ]