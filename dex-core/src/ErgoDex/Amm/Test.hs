module ErgoDex.Amm.Test where

import ErgoDex.Amm.PScripts
import qualified Plutus.V1.Ledger.Interval as Interval
import qualified Plutus.V1.Ledger.Value as Value
import PExtra.API
import qualified Data.Map       as Map1
import Data.Text (Text)
import qualified ErgoDex.Amm.PScripts as PScript
import Plutarch.Evaluate (evaluateScript)
import Plutarch (ClosedTerm, compile)
import Plutus.V1.Ledger.Api 
import Plutus.V1.Ledger.Scripts (Script (unScript), ScriptError, applyArguments)
import UntypedPlutusCore (DeBruijn, DefaultFun, DefaultUni, Program)
import PlutusTx (Data)
import ErgoDex.PContracts.PPool
import qualified ErgoDex.Contracts.Pool as P
import qualified ErgoDex.PContracts.PSwap as PS
import qualified ErgoDex.PContracts.PRedeem as PR
import qualified Ledger.Typed.Scripts.Validators as LV
import ErgoDex.PContracts.PPool as PP
import RIO
import           Control.Monad           (unless)
import           Data.Functor

import           Data.Either.Combinators (mapLeft)
import qualified Data.Set                as Set

import           Ledger          (Address, StakePubKeyHash, PaymentPubKeyHash, pubKeyHashAddress)
import qualified Ledger.Interval as Interval
import           Ledger.Value    (AssetClass)

import           ErgoDex.Types
import           ErgoDex.State
import           ErgoDex.Amm.Pool        (Pool(..), initPool)
import           ErgoDex.Amm.Constants
import           ErgoDex.Contracts.Types
import           CardanoTx.Models
import           ErgoDex.Contracts.Pool  (maxLqCapAmount)
import qualified Data.Set                as Set
import CardanoTx.Models (FullTxOut(..), FullTxIn(..), TxCandidate(..))
import ErgoDex.Contracts.Pool 
import ErgoDex.Contracts.Types (Coin(..))
import qualified ErgoDex.Amm.PoolSetup as PS
import qualified Ledger.Interval as Interval
import Plutus.V1.Ledger.Value (AssetClass(..))
import Plutus.V1.Ledger.Api (Datum(..), toBuiltinData, DatumHash(..), TxOutRef, Value, Redeemer(..))
import Plutus.V1.Ledger.Tx (TxInType(ConsumePublicKeyAddress))
import qualified Ledger as Ledger
import ErgoDex.Contracts.Pool as PR
import ErgoDex.Contracts.Pool as PRS
import qualified Data.Text.Encoding      as T
import qualified Data.ByteString.Base16  as Hex
import PlutusTx.Builtins.Internal (BuiltinByteString(..))
import Plutus.V1.Ledger.Api (CurrencySymbol(..), TokenName(..), Value(..), TxOutRef(..), TxId(..))
import PlutusTx.Builtins.Internal (BuiltinByteString(..))
import PlutusTx.AssocMap as Map
import qualified Ledger.Ada as Ada
import Plutarch.Api.V1          (mkMintingPolicy, mintingPolicySymbol, mkValidator)

cs :: CurrencySymbol
cs = "805fe1efcdea11f1e959eff4f422f118aa76dca2d0d797d184e487da"

nftTN :: TokenName
nftTN = "4e46545f546f6b656e5f6e65775f706f6f6c0a"

xTN :: TokenName
xTN = "415f546f6b656e5f6e65775f706f6f6c0a"

yTN :: TokenName
yTN = "425f546f6b656e5f6e65775f706f6f6c0a"

lqTN :: TokenName
lqTN = "6572676f6c6162736c70746f6b656e"



pPoolConfig :: P.PoolConfig
pPoolConfig = P.PoolConfig (Value.AssetClass (cs, nftTN)) (Value.AssetClass (cs, xTN)) (Value.AssetClass (cs, yTN)) (Value.AssetClass (cs, lqTN)) 100



pPoolIn :: TxInInfo
pPoolIn =
  TxInInfo
    { txInInfoOutRef = ref
    , txInInfoResolved = pPoolOut
    }

pPoolOut :: TxOut
pPoolOut =
  TxOut
    { txOutAddress   = undefined --Address (ScriptCredential mkPoolValidator) Nothing
    , txOutValue     = pPoolValueBefore
    , txOutDatumHash = Just datum1
    }

pOrderIn :: TxInInfo
pOrderIn =
  TxInInfo
    { txInInfoOutRef = ref
    , txInInfoResolved = pOrderOut
    }

pOrderOut :: TxOut
pOrderOut =
  TxOut
    { txOutAddress   = undefined -- Address (ScriptCredential mkPoolValidator) Nothing
    , txOutValue     = pOrderValue
    , txOutDatumHash = Just datum1
    }

pRewardOut :: TxOut
pRewardOut =
  TxOut
    { txOutAddress   = Address (PubKeyCredential pPubKeyHashReward) Nothing
    , txOutValue     = pRewardValue
    , txOutDatumHash = Just datum1
    }

pPoolSwapOut :: TxOut
pPoolSwapOut =
  TxOut
    { txOutAddress   = Address (PubKeyCredential pPubKeyHashReward) Nothing
    , txOutValue     = pRewardValue
    , txOutDatumHash = Just datum1
    }

nftCs :: CurrencySymbol
nftCs = "805fe1efcdea11f1e959eff4f422f118aa76dca2d0d797d184e487da"

nftTn :: TokenName
nftTn = "4e46545f546f6b656e5f6e65775f706f6f6c5f320a"

baseCs :: CurrencySymbol
baseCs = "805fe1efcdea11f1e959eff4f422f118aa76dca2d0d797d184e487da"

baseTn :: TokenName
baseTn = "415f546f6b656e5f6e65775f706f6f6c0a"

quoteCs :: CurrencySymbol
quoteCs = "805fe1efcdea11f1e959eff4f422f118aa76dca2d0d797d184e487da"

quoteTn :: TokenName
quoteTn = "425f546f6b656e5f6e65775f706f6f6c0a"

lpCs :: CurrencySymbol
lpCs = "805fe1efcdea11f1e959eff4f422f118aa76dca2d0d797d184e487da"

lpTn :: TokenName
lpTn = "544f4b454e5f4c505f4e45575f504f4f4c5f320a"

adaCs :: CurrencySymbol
adaCs = ""

adaTn :: TokenName
adaTn = ""

pPoolValueBefore :: Value
pPoolValueBefore = Value.singleton nftCs nftTn 1 <> Value.singleton baseCs baseTn 100 <> Value.singleton quoteCs quoteTn 100 <> Value.singleton lpCs lpTn 100



pOrderValue :: Value
pOrderValue = Value.singleton adaCs adaTn 1000000 <> Value.singleton quoteCs quoteTn 10

pRewardValue :: Value
pRewardValue = Value.singleton adaCs adaTn 1000000 <> Value.singleton quoteCs quoteTn 10

pPubKeyHashReward :: PubKeyHash
pPubKeyHashReward = "d74d26c5029cf290094fce1a0670da7369b9026571dfb977c6fa234f"

-- | Minting a single token
ref :: TxOutRef
ref = TxOutRef "a0" 0

purpose :: ScriptPurpose
purpose = Spending ref

--validator :: ValidatorHash
--validator = "a1"

datum1 :: DatumHash
datum1 = "d0"

sym :: CurrencySymbol
sym = mintingPolicySymbol $ mkRedeemPolicy  pPoolConfig

signatories :: [PubKeyHash]
signatories = [pPubKeyHashReward]

ine::Integer 
ine = 0

zeroRedeemer :: Redeemer 
zeroRedeemer = Redeemer . toBuiltinData $ ine

txCandidate :: TxCandidate
txCandidate = TxCandidate {
  txCandidateInputs = Set.fromList [pPoolTxIn1],
  txCandidateOutputs = [poolOutCandidate],
  txCandidateValueMint = MintValue mint,
  txCandidateMintInputs = MintInputs (Set.singleton swapPolicy) (Map1.singleton 0 zeroRedeemer),
  txCandidateChangePolicy = Just $ ReturnTo (Address (PubKeyCredential pPubKeyHashReward) Nothing),
  txCandidateValidRange   = Interval.always,
  txCandidateSigners = []
}

-- data MintInputs = MintInputs
--   { mintInputsPolicies  :: Set.Set MintingPolicy
--   , mintInputsRedeemers :: Map.Map Integer Redeemer
--   } deriving (Show, Eq, Generic, FromJSON, ToJSON)

-- data TxCandidate = TxCandidate
--   { txCandidateInputs       :: Set.Set FullTxIn
--   , txCandidateOutputs      :: [TxOutCandidate]
--   , txCandidateValueMint    :: MintValue
--   , txCandidateMintInputs   :: MintInputs
--   , txCandidateChangePolicy :: Maybe ChangePolicy
--   , txCandidateValidRange   :: SlotRange
--   , txCandidateSigners      :: [PaymentPubKeyHash]
--   } deriving (Show, Eq, Generic, FromJSON, ToJSON)
----------------------------------------------------------------------------------------------------------

mkPoolDatum' :: PRS.PoolConfig
mkPoolDatum' =
  PRS.PoolConfig
    { poolNft = AssetClass (currencySymbolName', poolNft')
    , poolX = AssetClass (currencySymbolName', poolX')
    , poolY = AssetClass (currencySymbolName', poolY')
    , poolLq = AssetClass (currencySymbolName', poolLq')
    , poolFeeNum = 1000
    }

pd' :: Datum
pd' = Datum $ toBuiltinData $ mkPoolDatum'

pdh' :: DatumHash
pdh' = Ledger.datumHash pd'

rd' :: Redeemer
rd' = Redeemer $ toBuiltinData PR.Swap

pPoolTxIn1 :: FullTxIn
pPoolTxIn1 =
  FullTxIn
    { fullTxInTxOut = pPoolOut1
    , fullTxInType = Ledger.ConsumeScriptAddress PScript.poolValidator (mkAllowedActions1 pPoolConfig) (swapDatum1 pPoolConfig)
    }

pPoolOut1 :: FullTxOut
pPoolOut1 =
  FullTxOut
    { fullTxOutRef = lpTxRef
    , fullTxOutAddress = (Address poolCred Nothing)
    , fullTxOutValue = 
           (mkTokenValue' currencySymbolName' poolLq' 9223372036854775807)
        <> (mkTokenValue' currencySymbolName' poolNft' 1)
        <> (mkTokenValue' currencySymbolName' poolX' 100)
        <> (mkTokenValue' currencySymbolName' poolY' 100)
        <> (lpAdaTxAmount)
    , fullTxOutDatumHash = Just pdh'
    , fullTxOutDatum = Just pd'
    }

poolOutCandidate =
  TxOutCandidate
    { txOutCandidateAddress = (Address poolCred Nothing)
    , txOutCandidateValue   =
           (mkTokenValue' currencySymbolName' poolLq' 9223372036854775807)
        <> (mkTokenValue' currencySymbolName' poolNft' 1)
        <> (mkTokenValue' currencySymbolName' poolX' 100)
        <> (mkTokenValue' currencySymbolName' poolY' 100)
        <> (lpAdaTxAmount)
    , txOutCandidateDatum   = Just pd'
    }

swapPolicy = mkSwapPolicy mkPoolDatum'
depositPolicy = mkDepositPolicy mkPoolDatum'
redeemPolicy = mkRedeemPolicy mkPoolDatum'

swapSymbol = mintingPolicySymbol swapPolicy
redeemSymbol = mintingPolicySymbol redeemPolicy
depositSymbol = mintingPolicySymbol depositPolicy

mint :: Value
mint = Value.singleton swapSymbol "6572676f6c6162736c70746f6b656e" 1

allowedActions = mkAllowedActions mkPoolDatum'

poolCred = ScriptCredential $ LV.validatorHash $ LV.unsafeMkTypedValidator $ PScript.poolValidator

currencySymbolName' :: CurrencySymbol
currencySymbolName' = mkCurrencySymbol' "805fe1efcdea11f1e959eff4f422f118aa76dca2d0d797d184e487da"

poolNft' :: TokenName
poolNft' = mkTokenName' "4e46545f546f6b656e5f6e65775f706f6f6c0a"

poolX' :: TokenName
poolX' = mkTokenName' "415f546f6b656e5f6e65775f706f6f6c0a"

poolY' :: TokenName
poolY' = mkTokenName' "425f546f6b656e5f6e65775f706f6f6c0a"

poolLq' :: TokenName
poolLq' = mkTokenName' "6572676f6c6162736c70746f6b656e"

unsafeFromEither :: (Show b) => Either b a -> a
unsafeFromEither (Left err)    = Prelude.error ("Err:" ++ show err)
unsafeFromEither (Right value) = value

mkByteString :: Text -> ByteString
mkByteString input = unsafeFromEither (Hex.decode . T.encodeUtf8 $ input)

mkTokenName' :: Text -> TokenName
mkTokenName' input = TokenName (BuiltinByteString $ mkByteString input)

mkCurrencySymbol' :: Text -> CurrencySymbol
mkCurrencySymbol' input = CurrencySymbol (BuiltinByteString $ mkByteString input)

mkAdaValue' :: Integer -> Value
mkAdaValue' count =
  Value $ Map.fromList [(Ada.adaSymbol, Map.singleton Ada.adaToken count)]

mkTxOutRef' :: Text -> Integer -> TxOutRef
mkTxOutRef' hash index = TxOutRef (TxId (BuiltinByteString $ mkByteString hash)) index

mkTokenValue' :: CurrencySymbol -> TokenName -> Integer -> Value
mkTokenValue' cs tn amount =
  Value $ Map.fromList [(cs, Map.singleton tn amount)]

lpAdaTxAmount :: Value
lpAdaTxAmount = mkAdaValue' 1517208

lpTxRef :: TxOutRef
lpTxRef = mkTxOutRef' "80f95be831a63d35b6a28b372fb82f608331e66e2b247082f9c5ef44c69bfb49" 1