{-# LANGUAGE TypeOperators #-}
module WalletAPI.CollateralOutputs
  ( CollateralOutputs(..)
  , mkWalletOutputs
  , mkWalletOutputs'
  , mkPersistentWalletOutputs
  ) where

import           RIO
import qualified Data.Set                as Set
import           Data.ByteArray.Encoding (Base(..), convertToBase)
import qualified Data.Text.Encoding      as T
import           Control.Retry
import           Control.Monad.Catch

import           Ledger
import           Plutus.V1.Ledger.Value
import           Cardano.Api            hiding (Value)

import           CardanoTx.Models
import           WalletAPI.UtxoStore
import           WalletAPI.Vault
import           System.Logging.Hlog

import           Explorer.Service
import qualified Explorer.Types    as Explorer
import qualified Explorer.Models   as Explorer
import qualified Explorer.Class    as Explorer
import           Explorer.Types    (PaymentCred)
import           Explorer.Models   (Paging, Items (items))

import           Algebra.Natural
import           WalletAPI.CollateralsStoreConfig    (CollateralsStoreConfig(..))
import           Control.Monad.Trans.Resource        (MonadResource)
import           Ledger.Ada (lovelaceOf, toValue, getLovelace)

data CollateralOutputs f = CollateralOutputs
  -- Contains only utxo with ada. Used for collaterals.
  { selectCollateralsStrict :: Ada -> f (Maybe (Set.Set FullTxOut))
  }

mkPersistentWalletOutputs ::
  ( MonadIO i
  , MonadIO f
  , MonadMask f
  , MonadResource i
  ) => (f ~> i) -> MakeLogging i f -> CollateralsStoreConfig -> Explorer f -> Vault f -> i (CollateralOutputs f)
mkPersistentWalletOutputs fToI mkLogging@MakeLogging{..} cfg@CollateralsStoreConfig{..} explorer vaultF = do
  let Vault{..} = fmapK fToI vaultF
  logging <- forComponent "CollateralOutputs"
  ustore  <- mkPersistentUtxoStoreWithCache mkLogging collateralsStorePath createIfMissing
  pkh     <- getPaymentKeyHash
  fToI $ filterSpentedUtxos ustore explorer
  pure $ CollateralOutputs
    { selectCollateralsStrict = selectCollateralsStrict'' logging cfg explorer ustore pkh True
    }

filterSpentedUtxos :: (Monad f) => UtxoStore f -> Explorer f -> f ()
filterSpentedUtxos UtxoStore{..} Explorer{..} = do
  allUtxos <- getUtxos
  (\FullTxOut{..} -> getOutput fullTxOutRef >>= (\case
      Just _  -> pure ()
      Nothing -> dropUtxos $ Set.fromList [fullTxOutRef]
    )) `traverse` toList allUtxos
  pure ()

mkWalletOutputs :: (MonadIO i, MonadIO f, MonadMask f) => MakeLogging i f -> CollateralsStoreConfig -> Explorer f -> Hash PaymentKey -> i (CollateralOutputs f)
mkWalletOutputs mkLogging@MakeLogging{..} cfg explorer pkh = do
  logging <- forComponent "CollateralOutputs"
  ustore  <- mkUtxoStore mkLogging
  pure $ CollateralOutputs
    { selectCollateralsStrict = selectCollateralsStrict'' logging cfg explorer ustore pkh True
    }

mkWalletOutputs' :: forall i f. (MonadIO i, MonadIO f, MonadMask f) => (f ~> i) -> MakeLogging i f -> CollateralsStoreConfig -> Explorer f -> Vault f -> i (CollateralOutputs f)
mkWalletOutputs' fToI mklogging cfg explorer vaultF = do
  let Vault{..}  = fmapK fToI vaultF
  getPaymentKeyHash >>= mkWalletOutputs mklogging cfg explorer

selectCollateralsStrict'' :: (MonadIO f, MonadMask f) => Logging f -> CollateralsStoreConfig -> Explorer f -> UtxoStore f -> Hash PaymentKey -> Bool -> Ada -> f (Maybe (Set.Set FullTxOut))
selectCollateralsStrict'' logging cfg@CollateralsStoreConfig{..} explorer ustore@UtxoStore{..} pkh strict requiredValue = do
  let
    fetchUtxos offset limit accValue = do
      let
        paging  = Explorer.Paging offset limit
        mkPCred = Explorer.PaymentCred . T.decodeUtf8 . convertToBase Base16 . serialiseToRawBytes
      utxoBatch <- getUnspentOutputsByPCredWithRetry logging explorer (mkPCred pkh) paging
      let
        adaUtxos    = Explorer.toCardanoTx <$> Explorer.containsOnlyAda `filter` items utxoBatch
        adaInUtxos = foldl (\acc utxo -> adaValue utxo <> acc) (lovelaceOf 0) adaUtxos
      putUtxos (Set.fromList adaUtxos)
      let entriesLeft = Explorer.total utxoBatch - (offset + limit)

      if entriesLeft > 0 && minimumAdaCapacity >= getLovelace (adaInUtxos <> accValue)
      then fetchUtxos (offset + limit) limit (adaInUtxos <> accValue)
      else pure ()

    collect :: [FullTxOut] -> Value -> [FullTxOut] -> Maybe [FullTxOut]
    collect acc valueAcc outs =
      case outs of
        fout@FullTxOut{..} : tl | valueAcc `lt` toValue requiredValue ->
          collect (fout : acc) (fullTxOutValue <> valueAcc) tl
        [] | valueAcc `lt` toValue requiredValue ->
          Nothing
        _ ->
          Just acc

  utxos <- getUtxos
  case collect [] mempty (Set.elems utxos) of
    Just outs -> pure $ Just $ Set.fromList outs
    Nothing   -> fetchUtxos 0 batchSize (lovelaceOf 0) >> selectCollateralsStrict'' logging cfg explorer ustore pkh strict requiredValue
      where batchSize = 400

getUnspentOutputsByPCredWithRetry :: (MonadIO f, MonadMask f) => Logging f -> Explorer f -> PaymentCred -> Paging -> f (Items Explorer.FullTxOut)
getUnspentOutputsByPCredWithRetry Logging{..} Explorer{..} cred paging = do
  let backoff = constantDelay 1000000
  recoverAll backoff (\rs -> infoM ("RetryStatus for getUnspentOutputsByPCredWithRetry " ++ show rs) >> getUnspentOutputsByPCred cred paging)