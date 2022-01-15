{-# LANGUAGE RecordWildCards #-}

module Explorer.Models where

import           Data.Aeson        (FromJSON)
import           Data.Aeson.Types
import           Data.String       (IsString(..))
import qualified Data.Text         as T
import           GHC.Generics

import qualified Ledger                 as P
import qualified Plutus.V1.Ledger.Value as Value
import           Explorer.Types
import           Explorer.Class
import qualified CardanoTx.Models       as Tx

import qualified Data.Set        as Set
import           Numeric.Natural

import qualified Cardano.Api as Api
import           Cardano.Api.Shelley   (ProtocolParameters(..), PoolId(..))
import qualified Ouroboros.Consensus.HardFork.History as History
import           Ouroboros.Consensus.HardFork.History.Summary as HSummary
import           Ouroboros.Consensus.HardFork.History.EraParams as EP
import           Ouroboros.Consensus.BlockchainTime.WallClock.Types
import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.Util.Counting
import           Cardano.Api.Shelley (Hash (ScriptDataHash), KESPeriod (KESPeriod),
                   OperationalCertificateIssueCounter (OperationalCertificateIssueCounter),
                   PlutusScript (PlutusScriptSerialised), ProtocolParameters (ProtocolParameters),
                   StakeCredential (StakeCredentialByKey), StakePoolKey)
import Cardano.Crypto.Seed

data SystemEnv = SystemEnv
  { pparams'           :: ProtocolParameters
  , network'           :: Api.NetworkId
  , sysstart'          :: SystemStart
  , pools'             :: Set.Set PoolId
  , eraHistory'        :: Api.EraHistory Api.CardanoMode
  , collateralPercent' :: Int
  } deriving (Show, Generic)

doTest :: PoolId
doTest =
  let
    sk = Api.deterministicSigningKey Api.AsStakePoolKey (mkSeedFromBytes "736b6f766f726f64612047677572646120626f726f64612070726f766f646120")
    vk = Api.getVerificationKey sk
  in Api.verificationKeyHash vk
 -- Api.verificationKeyHash $ Api.getVerificationKey <$> (Api.deterministicSigningKey Api.AsStakePoolKey 100)

instance Show (Api.EraHistory Api.CardanoMode) where
  show _ = "This is era history"

instance FromJSON SystemEnv where
  parseJSON = withObject "SystemEnv" $ \o -> do
    pparams'           <- o .: "pparams"
    sysstart'          <- o .: "sysstart"
    collateralPercent' <- o .: "collateralPercent"
    return 
      SystemEnv
        { pparams' = pparams'
        , network'           = Api.Testnet $ Api.NetworkMagic 1097911063
        , sysstart'          = sysstart'
        , pools'             = Set.fromList [doTest]
        , eraHistory'        = (Api.EraHistory (Api.CardanoMode) (History.mkInterpreter $ Summary $ NonEmptyOne $ 
          EraSummary {
              eraStart  = initBound
            , eraEnd    = EraUnbounded
            , eraParams = EraParams {
                eraEpochSize  = EpochSize 100
              , eraSlotLength = mkSlotLength 100
              , eraSafeZone   = UnsafeIndefiniteSafeZone
              }
            }
          )
        )
        , collateralPercent' = collateralPercent'
        }

data Paging = Paging
  { offset :: Int
  , limit  :: Int
  } deriving (Show, Generic, FromJSON)

data Items a = Items
  { items :: [a]
  , total :: Int
  } deriving (Show, Generic, FromJSON)

data FullTxOut = FullTxOut
  { ref         :: OutRef
  , txHash      :: TxHash
  , index       :: Int
  , globalIndex :: Gix
  , addr        :: Addr
  , value       :: [OutAsset]
  , dataHash    :: Maybe P.DatumHash
  , data'       :: Maybe P.Datum
  } deriving (Show, Generic)

instance FromJSON FullTxOut where
  parseJSON = withObject "quickblueFullTxOut" $ \o -> do
    ref          <- OutRef <$> o .: "ref"
    txHash       <- TxHash <$> o .: "txHash"
    index        <- o .: "index"
    globalIndex  <- Gix <$> o .: "globalIndex"
    addr         <- Addr <$> o .: "addr"
    value        <- o .: "value"
    dataHash     <- o .:? "dataHash"
    data'        <- o .:? "data"
    return FullTxOut{..}


instance ToCardanoTx FullTxOut Tx.FullTxOut where
  toCardanoTx FullTxOut{..} = Tx.FullTxOut
    { fullTxOutRef       = toCardanoTx ref
    , fullTxOutAddress   = toCardanoTx addr
    , fullTxOutValue     = foldr (\a -> \acc -> acc <> toCardanoTx a) mempty value
    , fullTxOutDatumHash = dataHash
    , fullTxOutDatum     = data'
    }

data OutAsset = OutAsset
  { policy      :: PolicyId
  , name        :: AssetName
  , quantity    :: Integer
  } deriving (Show, Generic)

instance FromJSON OutAsset where
  parseJSON = withObject "quickblueOutAsset" $ \o -> do
    policy    <- PolicyId <$> o .: "policyId"
    name      <- AssetName <$> o .: "name"
    quantity  <- o .: "quantity"
    return OutAsset{..}

instance ToCardanoTx OutAsset P.Value where
  toCardanoTx OutAsset{..} = Value.singleton p n quantity
    where
      p = fromString $ T.unpack $ unPolicyId policy
      n = fromString $ T.unpack $ unAssetName name
