{-# LANGUAGE OverloadedStrings #-}

module Spec.Transaction where

import           Data.Functor    ((<&>))
import qualified Data.ByteString as BS
import           Data.Map        as Map
import           Data.Set        as Set

import Test.Tasty
import Test.Tasty.HUnit
import Hedgehog
import Hedgehog.Gen as Gen
import Hedgehog.Range as Range

import Gen.CardanoTx
import Spec.Network

import qualified Cardano.Api as C

import NetworkAPI.Env
import SubmitAPI.Internal.Transaction
import CardanoTx.Models
import CardanoTx.Interop as Interop

inputsOrderPreservedBuildTxBody :: Property
inputsOrderPreservedBuildTxBody = property $ do
  txc <- forAll genPlainTxCandidate
  ctx <- buildTxBodyContent staticProtocolParams (network staticSystemEnv) mempty txc
  let
    Right txb       = C.makeTransactionBody ctx
    candidateInputs = Set.elems (txCandidateInputs txc) <&> (fullTxOutRef . fullTxInTxOut)
    balancedInputs  = Interop.extractCardanoTxBodyInputs txb
  balancedInputs === candidateInputs

outputsOrderPreservedBuildTxBody :: Property
outputsOrderPreservedBuildTxBody = property $ do
  txc <- forAll genPlainTxCandidate
  ctx <- buildTxBodyContent staticProtocolParams (network staticSystemEnv) mempty txc
  let
    Right txb        = C.makeTransactionBody ctx
    candidateOutputs = zip [0..] $ txCandidateOutputs txc
    balancedOutputs  = Interop.extractCardanoTxBodyOutputs txb
  balancedOutputs === candidateOutputs

buildTxBodyTests :: IO Bool
buildTxBodyTests = checkParallel $ Group "BuildTxBody"
  [ ("inputs_order_preserved", inputsOrderPreservedBuildTxBody)
  , ("outputs_order_preserved", outputsOrderPreservedBuildTxBody)
  ]

inputsOrderPreservedContent :: Property
inputsOrderPreservedContent = property $ do
  txc <- forAll genPlainTxCandidate
  ctx <- buildTxBodyContent staticProtocolParams (network staticSystemEnv) mempty txc
  let
    candidateInputs = Set.elems (txCandidateInputs txc) <&> (fullTxOutRef . fullTxInTxOut)
    balancedInputs  = Interop.extractCardanoTxContentInputs ctx
  balancedInputs === candidateInputs

outputsOrderPreservedContent :: Property
outputsOrderPreservedContent = property $ do
  txc <- forAll genPlainTxCandidate
  ctx <- buildTxBodyContent staticProtocolParams (network staticSystemEnv) mempty txc
  let
    candidateOutputs = zip [0..] $ txCandidateOutputs txc
    balancedOutputs  = Interop.extractCardanoTxContentOutputs ctx
  balancedOutputs === candidateOutputs

buildTxBodyContentTests :: IO Bool
buildTxBodyContentTests = checkParallel $ Group "BuildTxBodyContent"
  [ ("inputs_order_preserved", inputsOrderPreservedContent)
  , ("outputs_order_preserved", outputsOrderPreservedContent)
  ]

inputsOrderPreservedBalancing :: Property
inputsOrderPreservedBalancing = property $ do
  txc <- forAll genPlainTxCandidate
  (C.BalancedTxBody txb _ _) <- buildBalancedTx staticSystemEnv (ChangeAddress stableAddress) mempty txc
  let
    candidateInputs = Set.elems (txCandidateInputs txc) <&> (fullTxOutRef . fullTxInTxOut)
    balancedInputs  = Interop.extractCardanoTxBodyInputs txb
  balancedInputs === candidateInputs

outputsOrderPreservedBalancing :: Property
outputsOrderPreservedBalancing = property $ do
  txc <- forAll genPlainTxCandidate
  (C.BalancedTxBody txb _ _) <- buildBalancedTx staticSystemEnv (ChangeAddress stableAddress) mempty txc
  let
    candidateOutputs = zip [0..] $ txCandidateOutputs txc
    balancedOutputs  = Interop.extractCardanoTxBodyOutputs txb
  balancedOutputs === candidateOutputs

buildBalancedTxTests :: IO Bool
buildBalancedTxTests = checkParallel $ Group "BuildBalancedTx"
  [ ("inputs_order_preserved", inputsOrderPreservedBalancing)
  , ("outputs_order_preserved", outputsOrderPreservedBalancing)
  ]
