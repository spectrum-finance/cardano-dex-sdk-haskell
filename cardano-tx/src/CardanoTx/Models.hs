module CardanoTx.Models where

import           Data.Aeson (FromJSON, ToJSON)
import qualified Data.Set   as Set
import qualified Data.Map   as Map

import           Ledger                      hiding (TxIn)
import           Plutus.V1.Ledger.Credential (Credential (..))
import           Plutus.Script.Utils.Scripts
import qualified Ledger                      as P
import qualified Plutus.V2.Ledger.Tx         as PV2
import           GHC.Generics                (Generic)

import CardanoTx.ToPlutus (ToPlutus(..))
import Plutus.ChainIndex  (OutputDatum)

newtype ChangeAddress = ChangeAddress { getAddress :: Address }
  deriving (Eq, Generic)
  deriving newtype (Show, FromJSON, ToJSON)

-- Defines how a residual value (if any) should be handled
data ChangePolicy = ReturnTo Address
  deriving (Show, Eq, Generic, FromJSON, ToJSON)

newtype MintValue = MintValue { unMintValue :: Value }
  deriving (Eq, Generic)
  deriving newtype (Show, FromJSON, ToJSON)
  deriving Semigroup via Value
  deriving Monoid via Value

data TxOutDatum
  = KnownDatum Datum
  | KnownDatumHash DatumHash
  | EmptyDatum
  deriving (Show, Eq, Generic, FromJSON, ToJSON)

asTxOutDatumHash :: TxOutDatum -> Maybe DatumHash
asTxOutDatumHash (KnownDatum dt)     = Just $ datumHash dt
asTxOutDatumHash (KnownDatumHash dh) = Just dh
asTxOutDatumHash _                   = Nothing

asTxOutDatum :: TxOutDatum -> OutputDatum
asTxOutDatum (KnownDatum dt) = PV2.OutputDatum dt
asTxOutDatum _               = PV2.NoOutputDatum

-- TX output template
data TxOutCandidate = TxOutCandidate
  { txOutCandidateAddress   :: Address
  , txOutCandidateValue     :: Value
  , txOutCandidateDatum     :: TxOutDatum
  , txOutCandidateRefScript :: Maybe P.ScriptHash
  }
  deriving (Show, Eq, Generic, FromJSON, ToJSON)

instance ToPlutus TxOutCandidate PV2.TxOut where
  toPlutus TxOutCandidate{..} =
    PV2.TxOut txOutCandidateAddress txOutCandidateValue dh Nothing
      where dh = asTxOutDatum txOutCandidateDatum

instance Ord TxOutCandidate where
  compare TxOutCandidate{txOutCandidateAddress=rx} TxOutCandidate{txOutCandidateAddress=ry} = compare rx ry

data FullTxOut = FullTxOut
  { fullTxOutRef       :: TxOutRef
  , fullTxOutAddress   :: Address
  , fullTxOutValue     :: Value
  , fullTxOutDatum     :: TxOutDatum
  , fullTxOutScriptRef :: Maybe P.ScriptHash
  } deriving (Show, Eq, Generic, FromJSON, ToJSON)

mkFullTxOut :: TxOutRef -> TxOutCandidate -> FullTxOut
mkFullTxOut ref TxOutCandidate{..} =
    FullTxOut ref txOutCandidateAddress txOutCandidateValue txOutCandidateDatum txOutCandidateRefScript

instance ToPlutus FullTxOut PV2.TxOut where
  toPlutus FullTxOut{..} = PV2.TxOut fullTxOutAddress fullTxOutValue dh fullTxOutScriptRef
    where dh = asTxOutDatum fullTxOutDatum

instance Ord FullTxOut where
  compare FullTxOut{fullTxOutRef=rx} FullTxOut{fullTxOutRef=ry} = compare rx ry

data FullTxIn = FullTxIn
  { fullTxInTxOut :: FullTxOut
  , fullTxInType  :: TxInType
  } deriving (Show, Eq, Generic, FromJSON, ToJSON)

instance Ord FullTxIn where
  compare FullTxIn{fullTxInTxOut=foutx} FullTxIn{fullTxInTxOut=fouty} = compare foutx fouty

mkPkhTxIn :: FullTxOut -> FullTxIn
mkPkhTxIn fout = FullTxIn fout ConsumePublicKeyAddress

mkScriptTxIn :: FullTxOut -> Validator -> Redeemer -> FullTxIn
mkScriptTxIn fout@FullTxOut{..} v r =
  FullTxIn fout $ case (fullTxOutAddress, fullTxOutDatum) of
    (Address (ScriptCredential _) _, KnownDatum d) -> ConsumeScriptAddress PlutusV2 v r d
    _                                              -> ConsumeScriptAddress PlutusV2 v r unitDatum

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

instance Semigroup MintInputs where
  (<>) (MintInputs lInputs lRedeemers) (MintInputs rInputs rRedeemers) =
    MintInputs
      { mintInputsPolicies  = lInputs <> rInputs
      , mintInputsRedeemers = lRedeemers <> rRedeemers
      }

instance Monoid MintInputs where
  mempty =
    MintInputs
      { mintInputsPolicies  = Set.empty
      , mintInputsRedeemers = Map.empty
      }

mkMintInputs :: [(MintingPolicy, Redeemer)] -> MintInputs
mkMintInputs xs = MintInputs mps rs
  where (mps, rs) = foldr (\ (ix, (mp, r)) (mpsa, rsa) -> (Set.insert mp mpsa, Map.insert ix r rsa)) (mempty, mempty) (zip [0..] xs)

-- TX template without collaterals, fees, change etc.
data TxCandidate = TxCandidate
  { txCandidateInputs       :: Set.Set FullTxIn
  , txCandidateRefIns       :: [FullTxOut]      -- we are not going to consume those inputs, so they are represented as FullTxOut
  , txCandidateOutputs      :: [TxOutCandidate]
  , txCandidateValueMint    :: MintValue
  , txCandidateMintInputs   :: MintInputs
  , txCandidateChangePolicy :: Maybe ChangePolicy
  , txCandidateValidRange   :: SlotRange
  , txCandidateSigners      :: [PaymentPubKeyHash]
  } deriving (Show, Eq, Generic, FromJSON, ToJSON)