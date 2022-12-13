{-# LANGUAGE OverloadedStrings #-}

module Spec.Transaction where

import           Data.Functor    ((<&>))
import qualified Data.ByteString as BS
import           Data.Map        as Map
import qualified Data.List       as List
import           Data.Set        as Set

import Hedgehog
import Hedgehog.Gen        as Gen
import Hedgehog.Range      as Range
import Test.Tasty          (testGroup)
import Test.Tasty.Hedgehog as HH

import Gen.CardanoTx
import Spec.Network

import qualified Cardano.Api as C

import NetworkAPI.Types
import SubmitAPI.Internal.Transaction
import CardanoTx.Models
import CardanoTx.Interop as Interop
import Cardano.Api.Shelley (NetworkId(Mainnet))

inputsOrderPreservedBuildTxBody :: Property
inputsOrderPreservedBuildTxBody = property $ do
  txc <- forAll genPlainTxCandidate
  ctx <- buildTxBodyContent staticProtocolParams Mainnet mempty mempty txc
  let
    Right txb       = C.makeTransactionBody ctx
    candidateInputs = Set.elems (txCandidateInputs txc) <&> (fullTxOutRef . fullTxInTxOut)
    balancedInputs  = Interop.extractCardanoTxBodyInputs txb
  balancedInputs === candidateInputs

outputsOrderPreservedBuildTxBody :: Property
outputsOrderPreservedBuildTxBody = property $ do
  txc <- forAll genPlainTxCandidate
  ctx <- buildTxBodyContent staticProtocolParams Mainnet mempty mempty txc
  let
    Right txb        = C.makeTransactionBody ctx
    candidateOutputs = zip [0..] $ txCandidateOutputs txc
    balancedOutputs  = Interop.extractCardanoTxBodyOutputs txb
  balancedOutputs === candidateOutputs

buildTxBodyTests = testGroup "BuildTxBody"
  [ HH.testProperty "inputs_order_preserved" inputsOrderPreservedBuildTxBody
  , HH.testProperty "outputs_order_preserved" outputsOrderPreservedBuildTxBody
  ]

inputsOrderPreservedContent :: Property
inputsOrderPreservedContent = property $ do
  txc <- forAll genPlainTxCandidate
  ctx <- buildTxBodyContent staticProtocolParams Mainnet mempty mempty txc
  let
    candidateInputs = Set.elems (txCandidateInputs txc) <&> (fullTxOutRef . fullTxInTxOut)
    balancedInputs  = Interop.extractCardanoTxContentInputs ctx
  balancedInputs === candidateInputs

outputsOrderPreservedContent :: Property
outputsOrderPreservedContent = property $ do
  txc <- forAll genPlainTxCandidate
  ctx <- buildTxBodyContent staticProtocolParams Mainnet mempty mempty txc
  let
    candidateOutputs = zip [0..] $ txCandidateOutputs txc
    balancedOutputs  = Interop.extractCardanoTxContentOutputs ctx
  balancedOutputs === candidateOutputs

buildTxBodyContentTests = testGroup "BuildTxBodyContent"
  [ HH.testProperty "inputs_order_preserved" inputsOrderPreservedContent
  , HH.testProperty "outputs_order_preserved" outputsOrderPreservedContent
  ]

inputsOrderPreservedBalancing :: Property
inputsOrderPreservedBalancing = property $ do
  txc <- forAll genPlainTxCandidate
  (C.BalancedTxBody txb _ _) <- buildBalancedTx staticSystemEnv mempty Mainnet (ChangeAddress stableAddress) mempty txc
  let
    candidateInputs = Set.elems (txCandidateInputs txc) <&> (fullTxOutRef . fullTxInTxOut)
    balancedInputs  = Interop.extractCardanoTxBodyInputs txb
  balancedInputs === candidateInputs

outputsOrderPreservedBalancing :: Property
outputsOrderPreservedBalancing = property $ do
  txc <- forAll genPlainTxCandidate
  (C.BalancedTxBody txb _ _) <- buildBalancedTx staticSystemEnv mempty Mainnet (ChangeAddress stableAddress) mempty txc
  let
    candidateOutputs = zip [0..] $ txCandidateOutputs txc
    balancedOutputs  = Interop.extractCardanoTxBodyOutputs txb
  List.init balancedOutputs === candidateOutputs

buildBalancedTxTests = testGroup "BuildBalancedTx"
  [ HH.testProperty "inputs_order_preserved" inputsOrderPreservedBalancing
  , HH.testProperty "outputs_order_preserved" outputsOrderPreservedBalancing
  ]
