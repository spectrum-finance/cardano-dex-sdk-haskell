module ErgoDex.ScriptsValidators where

import Codec.Serialise 
  ( deserialise )
import RIO
  ( MonadIO (liftIO), Generic )
import Dhall
  ( FromDhall, input, auto )

import qualified Data.ByteString.Lazy as BSL

import qualified Plutus.V2.Ledger.Api as PV2

import ErgoDex.Amm.PoolActions 
  ( AmmValidators (..) )
import ErgoDex.Validators 
  ( V1, PoolValidator (..), OrderValidator (..) )
import System.Logging.Hlog
import CardanoTx.Models (FullTxOut(..))
import ErgoDex.State (Confirmed(Confirmed), OnChain (OnChain))
import ErgoDex.Amm.Pool (Pool(Pool))
import ErgoDex.Class (parseFromLedger)
import Plutus.Script.Utils.V2.Address (mkValidatorAddress)

data ScriptsConfig = ScriptsConfig
  { swapScriptPath    :: !FilePath
  , depositScriptPath :: !FilePath
  , redeemScriptPath  :: !FilePath
  , poolScriptPath    :: !FilePath
  } deriving (Generic, FromDhall)

data ScriptsValidators = ScriptsValidators
  { swapValidator    :: PV2.Validator 
  , depositValidator :: PV2.Validator
  , redeemValidator  :: PV2.Validator
  , poolValidator    :: PV2.Validator
  }

parsePool :: (MonadIO m) => Logging m -> ScriptsValidators -> FullTxOut -> m (Maybe (Confirmed (OnChain Pool)))
parsePool Logging{..} ScriptsValidators{poolValidator} out@FullTxOut{..} = do
  let
    pool        = parseFromLedger out :: Maybe (OnChain Pool)
    poolAddress = mkValidatorAddress poolValidator
  if fullTxOutAddress == poolAddress
    then case pool of
      Just a    -> do
        infoM ("Pool found in: " ++ show out)
        pure $ Just $ Confirmed out a
      _         -> do
        infoM ("Pool not found in: " ++ show out)
        pure Nothing
  else pure Nothing

mkScriptsValidators :: (MonadIO m) => ScriptsConfig -> m ScriptsValidators
mkScriptsValidators ScriptsConfig{..} = do
  swapValidator    <- readValidatorFromFile swapScriptPath
  redeemValidator  <- readValidatorFromFile redeemScriptPath
  depositValidator <- readValidatorFromFile depositScriptPath
  poolValidator    <- readValidatorFromFile poolScriptPath
  pure $ ScriptsValidators{..}
      
readValidatorFromFile :: (MonadIO m) => FilePath -> m PV2.Validator
readValidatorFromFile path = do
  bytes <- liftIO $ BSL.readFile path
  pure $ deserialise bytes
