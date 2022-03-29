{-# LANGUAGE OverloadedStrings #-}

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
import Spec.Network

import qualified Cardano.Api as C

import SubmitAPI.Internal.Transaction
import CardanoTx.Models
import CardanoTx.Interop as Interop
import GHC.Base (Bool)

inputsOrderPreserved :: Property
inputsOrderPreserved = property $ do
  txc <- forAll genPlainTxCandidate
  _   <- evalIO $ Prelude.print txc
  (C.BalancedTxBody txb _ _) <- buildBalancedTx staticSystemEnv (ChangeAddress stableAddress) mempty txc
  let
    candidateInputs = txCandidateInputs txc <&> (fullTxOutRef . fullTxInTxOut)
    balancedInputs  = Interop.extractCardanoTxBodyInputs txb
  balancedInputs === candidateInputs

outputsOrderPreserved :: Property
outputsOrderPreserved = property $ do
  txc <- forAll genPlainTxCandidate
  (C.BalancedTxBody txb _ _) <- buildBalancedTx staticSystemEnv (ChangeAddress stableAddress) mempty txc
  let
    candidateOutputs = zip [0..] $ txCandidateOutputs txc
    balancedOutputs  = Interop.extractCardanoTxBodyOutputs txb
  balancedOutputs === candidateOutputs

buildBalancedTxTests :: IO Bool
buildBalancedTxTests = checkParallel $ Group "BuildBalancedTx"
  [ ("inputs_order_preserved", inputsOrderPreserved)
  , ("outputs_order_preserved", outputsOrderPreserved)
  ]
