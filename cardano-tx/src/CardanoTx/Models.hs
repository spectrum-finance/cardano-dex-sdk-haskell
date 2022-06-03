module CardanoTx.Models where

import           Data.Aeson (FromJSON, ToJSON)
import qualified Data.Set   as Set
import qualified Data.Map   as Map
import qualified Data.Text               as T

import           Ledger                      hiding (TxIn)
import           Plutus.V1.Ledger.Credential (Credential (..))
import qualified Ledger                      as P
import qualified Ledger.Constraints.OffChain as P
import           GHC.Generics                (Generic)

import CardanoTx.ToPlutus (ToPlutus(..))

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

asTxOutDatum :: TxOutDatum -> Maybe Datum
asTxOutDatum (KnownDatum dt) = Just dt
asTxOutDatum _               = Nothing

-- TX output template
data TxOutCandidate = TxOutCandidate
  { txOutCandidateAddress :: Address
  , txOutCandidateValue   :: Value
  , txOutCandidateDatum   :: TxOutDatum
  }
  deriving (Show, Eq, Generic, FromJSON, ToJSON)

instance ToPlutus TxOutCandidate P.TxOut where
  toPlutus TxOutCandidate{..} =
    P.TxOut txOutCandidateAddress txOutCandidateValue dh
      where dh = asTxOutDatumHash txOutCandidateDatum

instance Ord TxOutCandidate where
  compare TxOutCandidate{txOutCandidateAddress=rx} TxOutCandidate{txOutCandidateAddress=ry} = compare rx ry

data FullTxOut = FullTxOut
  { fullTxOutRef     :: TxOutRef
  , fullTxOutAddress :: Address
  , fullTxOutValue   :: Value
  , fullTxOutDatum   :: TxOutDatum
  } deriving (Show, Eq, Generic, FromJSON, ToJSON)

mkFullTxOut :: TxOutRef -> TxOutCandidate -> FullTxOut
mkFullTxOut ref TxOutCandidate{..} =
    FullTxOut ref txOutCandidateAddress txOutCandidateValue txOutCandidateDatum

instance ToPlutus FullTxOut P.TxOut where
  toPlutus FullTxOut{..} = P.TxOut fullTxOutAddress fullTxOutValue dh
    where dh = asTxOutDatumHash fullTxOutDatum

instance Ord FullTxOut where
  compare FullTxOut{fullTxOutRef=rx} FullTxOut{fullTxOutRef=ry} = compare rx ry

data FullTxIn = FullTxIn
  { fullTxInTxOut :: FullTxOut
  , fullTxInType  :: TxInType
  } deriving (Show, Eq, Generic, FromJSON, ToJSON)

instance Ord FullTxIn where
  compare FullTxIn{fullTxInTxOut=foutx} FullTxIn{fullTxInTxOut=fouty} = compare foutx fouty

toScriptOutput :: FullTxIn -> Maybe P.ScriptOutput
toScriptOutput FullTxIn{fullTxInTxOut=FullTxOut{fullTxOutValue}, fullTxInType=ConsumeScriptAddress v _ d} =
  Just $ P.ScriptOutput (validatorHash v) fullTxOutValue (datumHash d)
toScriptOutput _ = Nothing

mkPkhTxIn :: FullTxOut -> FullTxIn
mkPkhTxIn fout = FullTxIn fout ConsumePublicKeyAddress

mkScriptTxIn :: FullTxOut -> Validator -> Redeemer -> FullTxIn
mkScriptTxIn fout@FullTxOut{..} v r =
  FullTxIn fout $ case (fullTxOutAddress, fullTxOutDatum) of
    (Address (ScriptCredential _) _, KnownDatum d) -> ConsumeScriptAddress v r d
    _                                              -> ConsumeScriptAddress v r unitDatum

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
  , txCandidateOutputs      :: [TxOutCandidate]
  , txCandidateValueMint    :: MintValue
  , txCandidateMintInputs   :: MintInputs
  , txCandidateChangePolicy :: Maybe ChangePolicy
  , txCandidateValidRange   :: SlotRange
  , txCandidateSigners      :: [PaymentPubKeyHash]
  } deriving (Show, Eq, Generic, FromJSON, ToJSON)

-- 28 bytes hash represented as a hex string
newtype Hash28 = Hash28 { unHash28 :: T.Text }
  deriving (Eq, Generic)
  deriving newtype (Show, FromJSON)

-- TX hash32 represented as a hex string
newtype TxHash = TxHash { unTxHash :: T.Text }
  deriving (Eq, Generic)
  deriving newtype (Show, FromJSON)

-- Block hash32 represented as a hex string
newtype BlockHash = BlockHash { unBlockHash :: T.Text }
  deriving (Eq, Generic)
  deriving newtype (Show, FromJSON)

data CompletedRedeemer = CompletedRedeemer
  { redeemer   :: Maybe Redeemer
  , scriptHash :: Hash28
  } deriving (Show, Eq, Generic, FromJSON)

data CompletedTxIn = CompletedTxIn
  { out      :: FullTxOut
  , redeemer :: Maybe CompletedRedeemer
  } deriving (Show, Eq, Generic, FromJSON)

data CompletedTx = CompletedTx
  { blockHash  :: BlockHash
  , blockIndex :: Int
  , hash       :: TxHash
  , inputs     :: [FullTxIn]
  , outputs    :: [FullTxOut]
  } deriving (Show, Eq, Generic, FromJSON)