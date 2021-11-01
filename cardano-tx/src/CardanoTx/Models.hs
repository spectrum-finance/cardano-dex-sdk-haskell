module CardanoTx.Models where

import           Data.Functor
import           Data.Aeson     (FromJSON, ToJSON)
import qualified Data.Set       as Set

import           Ledger
import           Ledger.Scripts (datumHash)
import qualified Ledger         as P
import           GHC.Generics   (Generic)

import CardanoTx.ToPlutus

-- Defines how a residual value (if any) should be handled
data ChangePolicy = ReturnTo Address
  deriving (Show, Eq, Generic, FromJSON, ToJSON)

newtype MintValue = MintValue { unMintValue :: Value }
  deriving (Show, Eq, Generic, FromJSON, ToJSON)

-- TX output template
data TxOutCandidate = TxOutCandidate
  { txOutCandidateAddress  :: Address
  , txOutCandidateValue    :: Value
  , txOutCandidateDatum    :: Maybe Datum
  } deriving (Show, Eq, Generic, FromJSON, ToJSON)

instance ToPlutus TxOutCandidate P.TxOut where
  toPlutus TxOutCandidate{..} =
    P.TxOut txOutCandidateAddress txOutCandidateValue dh
      where dh = txOutCandidateDatum <&> datumHash

data FullTxOut = FullTxOut
  { fullTxOutRef       :: TxOutRef
  , fullTxOutAddress   :: Address
  , fullTxOutValue     :: Value
  , fullTxOutDatumHash :: Maybe DatumHash
  , fullTxOutDatum     :: Maybe Datum
  } deriving (Show, Eq, Generic, FromJSON, ToJSON)

instance Ord FullTxOut where
  compare FullTxOut{fullTxOutRef=rx} FullTxOut{fullTxOutRef=ry} = compare rx ry

data FullTxIn = FullTxIn
  { fullTxInTxOut :: FullTxOut
  , fullTxInType  :: TxInType
  } deriving (Show, Eq, Ord, Generic, FromJSON, ToJSON)

instance ToPlutus FullTxIn P.TxIn where
  toPlutus FullTxIn{..} =
    P.TxIn (fullTxOutRef fullTxInTxOut) $ Just fullTxInType

data FullCollateralTxIn = FullCollateralTxIn
  { fullCollateralTxInTxOut :: FullTxOut
  } deriving (Show, Eq, Ord, Generic, FromJSON, ToJSON)

instance ToPlutus FullCollateralTxIn P.TxIn where
  toPlutus FullCollateralTxIn{fullCollateralTxInTxOut=FullTxOut{..}} =
    P.TxIn fullTxOutRef $ Just P.ConsumePublicKeyAddress

-- TX template without collaterals, fees, change etc.
data TxCandidate = TxCandidate
  { txCandidateInputs       :: Set.Set FullTxIn
  , txCandidateOutputs      :: [TxOutCandidate]
  , txCandidateValueMint    :: MintValue
  , txCandidateMintPolicies :: Set.Set MintingPolicy
  , txCandidateChangePolicy :: Maybe ChangePolicy
  , txCandidateValidRange   :: SlotRange
  } deriving (Show, Eq, Generic, FromJSON, ToJSON)
