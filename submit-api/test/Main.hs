module Main where

import qualified Data.Text.Encoding as E

import Test.Tasty
import Test.Tasty.HUnit

import Spec.Transaction
import System.Exit (exitFailure)
import Control.Monad (unless)
import CardanoTx.Address
import ErgoDex.Amm.Scripts
import qualified Ledger.Typed.Scripts.Validators as LV
import Cardano.Api

main :: IO ()
main = do
  res <- sequence
    [ buildBalancedTxTests
    ]
  unless (and res) exitFailure
