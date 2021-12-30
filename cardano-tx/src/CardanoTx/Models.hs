module CardanoTx.Models where

import           Data.Functor
import           Data.Aeson     (FromJSON, ToJSON)
import qualified Data.Set       as Set
import qualified Data.Map       as Map

import           Ledger
import           Plutus.V1.Ledger.Credential (Credential (..))
import qualified Ledger                      as P
import           GHC.Generics                (Generic)

import CardanoTx.ToPlutus

newtype ChangeAddress = ChangeAddress { getAddress :: Address }
  deriving (Eq, Generic)
  deriving newtype (Show, FromJSON, ToJSON)

-- Defines how a residual value (if any) should be handled
data ChangePolicy = ReturnTo Address
  deriving (Show, Eq, Generic, FromJSON, ToJSON)

newtype MintValue = MintValue { unMintValue :: Value }
  deriving (Eq, Generic)
  deriving newtype (Show, FromJSON, ToJSON)

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

instance ToPlutus FullTxOut P.TxOut where
  toPlutus FullTxOut{..} = P.TxOut fullTxOutAddress fullTxOutValue fullTxOutDatumHash

instance Ord FullTxOut where
  compare FullTxOut{fullTxOutRef=rx} FullTxOut{fullTxOutRef=ry} = compare rx ry

data FullTxIn = FullTxIn
  { fullTxInTxOut :: FullTxOut
  , fullTxInType  :: TxInType
  } deriving (Show, Eq, Ord, Generic, FromJSON, ToJSON)

mkScriptTxIn :: FullTxOut -> Validator -> Redeemer -> FullTxIn
mkScriptTxIn fout@FullTxOut{..} v r =
  FullTxIn fout $ case (fullTxOutAddress, fullTxOutDatum) of
    (Address (ScriptCredential _) _, Just d) -> ConsumeScriptAddress v r d
    _                                        -> ConsumeScriptAddress v r unitDatum

instance ToPlutus FullTxIn P.TxIn where
  toPlutus FullTxIn{..} =
    P.TxIn (fullTxOutRef fullTxInTxOut) $ Just fullTxInType

data FullCollateralTxIn = FullCollateralTxIn
  { fullCollateralTxInTxOut :: FullTxOut
  } deriving (Show, Eq, Ord, Generic, FromJSON, ToJSON)

instance ToPlutus FullCollateralTxIn P.TxIn where
  toPlutus FullCollateralTxIn{fullCollateralTxInTxOut=FullTxOut{..}} =
    P.TxIn fullTxOutRef $ Just P.ConsumePublicKeyAddress

data MintInputs = MintInputs
  { mintInputsPolicies  :: Set.Set MintingPolicy
  , mintInputsRedeemers :: Map.Map Integer Redeemer
  } deriving (Show, Eq, Generic, FromJSON, ToJSON)

mkMintInputs :: [(MintingPolicy, Redeemer)] -> MintInputs
mkMintInputs xs = MintInputs mps rs
  where (mps, rs) = foldr (\ (ix, (mp, r)) (mpsa, rsa) -> (Set.insert mp mpsa, Map.insert ix r rsa)) (mempty, mempty) (zip [0..] xs)

-- TX template without collaterals, fees, change etc.
data TxCandidate = TxCandidate
  { txCandidateInputs       :: Set.Set FullTxIn
  , txCandidateOutputs      :: [TxOutCandidate]
  , txCandidateValueMint    :: MintValue
  , txCandidateMintInputs   :: MintInputs
  , txCandidateChangePolicy :: Maybe ChangePolicy
  , txCandidateValidRange   :: SlotRange
  } deriving (Show, Eq, Generic, FromJSON, ToJSON)
