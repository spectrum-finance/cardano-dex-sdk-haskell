module Spec.Transaction where

import           Data.Functor    ((<&>))
import qualified Data.ByteString as BS
import           Data.Map        as Map

import Test.Tasty
import Test.Tasty.HUnit
import Hedgehog
import Hedgehog.Gen as Gen
import Hedgehog.Range as Range

import Gen.CardanoTx

buildBalancedTxTests = testGroup "Build balanced TX" []
