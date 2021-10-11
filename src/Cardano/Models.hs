{-# LANGUAGE TypeApplications #-}

module Cardano.Models where

import Data.Maybe

import Ledger
import Ledger.Tx
import Plutus.V1.Ledger.Credential
import Playground.Contract (FromJSON, ToJSON, Generic)

import Cardano.Types

data TxOutCandidate = TxOutCandidate
  { txOutCandidateAddress :: Address
  , txOutCandidateValue   :: Value
  , txOutCandidateDatum   :: Maybe Datum
  } deriving (Show, Eq, Generic, FromJSON, ToJSON)

data FullTxOut = FullTxOut
  { fullTxOutGix     :: Gix
  , fullTxOutRef     :: TxOutRef
  , fullTxOutAddress :: Address
  , fullTxOutValue   :: Value
  , fullTxOutDatum   :: Maybe Datum
  } deriving (Show, Eq, Generic, FromJSON, ToJSON)

data TxInTarget = Pay2Script | Pay2PubKey
  deriving (Show, Eq, Generic, FromJSON, ToJSON)

data FullTxIn = FullTxIn
  { fullTxInTxOut    :: FullTxOut
  , fullTxInTarget   :: TxInTarget
  , fullTxInRedeemer :: Maybe Redeemer
  } deriving (Show, Eq, Generic, FromJSON, ToJSON)

fromTxOut2TxIn :: FullTxOut -> Maybe Redeemer -> Maybe FullTxIn
fromTxOut2TxIn txOut@FullTxOut{..} redeemerM =
  case (fullTxOutAddress, redeemerM) of
    (Address{addressCredential = ScriptCredential _}, Just redeemer) -> Just $ FullTxIn
                                                                        { fullTxInTxOut    = txOut
                                                                        , fullTxInTarget   = Pay2Script
                                                                        , fullTxInRedeemer = Just redeemer
                                                                        }
    (Address{addressCredential = PubKeyCredential _}, Nothing)       -> Just $ FullTxIn
                                                                        { fullTxInTxOut    = txOut
                                                                        , fullTxInTarget   = Pay2PubKey
                                                                        , fullTxInRedeemer = Nothing
                                                                        }
    _                                                                -> Nothing

data TxCandidate = TxCandidate
  { txCandidateInputs  :: [FullTxIn]
  , txCandidateOutputs :: [TxOutCandidate]
  } deriving (Show, Eq, Generic, FromJSON, ToJSON)
