module Explorer.Models where

import           Data.Aeson.Types
import qualified Data.Set                as Set
import qualified Data.Either.Combinators as EC
import           Data.String             (IsString(..))
import qualified Data.Text               as T
import           GHC.Generics

import qualified Ledger                     as P
import qualified Plutus.V1.Ledger.Value     as Value
import qualified Plutus.V1.Ledger.Tx        as Tx
import qualified PlutusTx.Builtins.Internal as BI
import           Explorer.Types
import           Explorer.Class
import qualified CardanoTx.Models           as Tx
import           CardanoTx.Value
import qualified CardanoTx.Types            as Tx

import qualified Cardano.Api as Api
import           Cardano.Api.Shelley   (ProtocolParameters(..), PoolId, toPlutusData)
import qualified Ouroboros.Consensus.HardFork.History as History
import           Ouroboros.Consensus.HardFork.History.Summary as HSummary
import           Ouroboros.Consensus.HardFork.History.EraParams as EP
import           Ouroboros.Consensus.BlockchainTime.WallClock.Types
import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.Util.Counting

data SystemEnv = SystemEnv
  { pparams'           :: ProtocolParameters
  , network'           :: Api.NetworkId
  , sysstart'          :: SystemStart
  , pools'             :: Set.Set PoolId
  , eraHistory'        :: Api.EraHistory Api.CardanoMode
  } deriving (Show, Generic)

instance Show (Api.EraHistory Api.CardanoMode) where
  show _ = "Era history"

instance FromJSON SystemEnv where
  parseJSON = withObject "SystemEnv" $ \o -> do
    pparams'  <- o .: "pparams"
    sysstart' <- o .: "sysstart"
    return
      SystemEnv
        { pparams'    = pparams'
        , network'    = Api.Testnet $ Api.NetworkMagic 1097911063
        , sysstart'   = sysstart'
        , pools'      = Set.empty
        , eraHistory' = dummyEraHistory
        }

dummyEraHistory :: Api.EraHistory Api.CardanoMode
dummyEraHistory =
  Api.EraHistory
    Api.CardanoMode
    (History.mkInterpreter $ Summary $ NonEmptyOne $
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

data Paging = Paging
  { offset :: Int
  , limit  :: Int
  } deriving (Show, Generic, FromJSON)

data Items a = Items
  { items :: [a]
  , total :: Int
  } deriving (Show, Generic, FromJSON)

data FullTx = FullTx
  { blockHash  :: BlockHash
  , blockIndex :: Int
  , hash       :: TxHash
  , inputs     :: [FullTxIn]
  , outputs    :: [FullTxOut]
  } deriving (Show, Generic, FromJSON)

instance ToCardanoTx FullTx Tx.CompletedTx where
  toCardanoTx FullTx{..} =
    Tx.CompletedTx
      { blockHash = Tx.BlockHash $ unBlockHash blockHash
      , txIndex   = txIndex
      , hash      = Tx.TxHash $ unTxHash hash
      , inputs    = fmap toCardanoTx inputs
      , outputs   = fmap toCardanoTx outputs
      } 

data FullTxIn = FullTxIn
  { out :: FullTxOut
  } deriving (Show, Generic, FromJSON)

instance ToCardanoTx FullTxIn Tx.FullTxIn where
  toCardanoTx FullTxIn{..} =
    Tx.FullTxIn
      { fullTxInTxOut = toCardanoTx out
      -- actually, we don't need this field att all, so we keep it default
      , fullTxInType  = Tx.ConsumeSimpleScriptAddress
      }

data FullTxOut = FullTxOut
  { ref           :: OutRef
  , txHash        :: TxHash
  , index         :: Int
  , globalIndex   :: Gix
  , addr          :: Addr
  , value         :: [OutAsset]
  , dataHash      :: Maybe P.DatumHash
  , data'         :: Maybe P.Datum
  , spentByTxHash :: Maybe TxHash
  } deriving (Show, Generic)

instance FromJSON FullTxOut where
  parseJSON = withObject "quickblueFullTxOut" $ \o -> do
    ref           <- OutRef <$> o .: "ref"
    txHash        <- TxHash <$> o .: "txHash"
    index         <- o .: "index"
    globalIndex   <- Gix <$> o .: "globalIndex"
    addr          <- Addr <$> o .: "addr"
    value         <- o .: "value"
    dataHash      <- o .:? "dataHash"
    rawDataM      <- o .:? "data"
    spentByTxHash <- o .:? "spentByTxHash"
    let
      jsonDataM  = rawDataM >>= (EC.rightToMaybe . Api.scriptDataFromJson Api.ScriptDataJsonDetailedSchema)
      data'      = fmap (P.Datum . BI.dataToBuiltinData . toPlutusData) jsonDataM
    return FullTxOut{..}

instance ToCardanoTx FullTxOut Tx.FullTxOut where
  toCardanoTx FullTxOut{..} =
    Tx.FullTxOut
      { fullTxOutRef       = toCardanoTx ref
      , fullTxOutAddress   = toCardanoTx addr
      , fullTxOutValue     = foldr (\a acc -> unionVal acc (toCardanoTx a)) mempty value
      , fullTxOutDatum     = outDatum
      }
    where
      outDatum = case (data', dataHash) of
        (Just d, _)  -> Tx.KnownDatum d
        (_, Just dh) -> Tx.KnownDatumHash dh
        _            -> Tx.EmptyDatum

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