{-# LANGUAGE TypeOperators #-}
module WalletAPI.Utxos
  ( WalletOutputs(..)
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
import           Explorer.Models   (Paging, Items)

import           Algebra.Natural
import           WalletAPI.UtxoStoreConfig    (UtxoStoreConfig(..))
import           Control.Monad.Trans.Resource (MonadResource)
import           Spectrum.Prelude.HigherKind  (LiftK(..))

data WalletOutputs f = WalletOutputs
  -- Select UTxOs satisfying the given minumal Value.
  { selectUtxos       :: Value -> f (Maybe (Set.Set FullTxOut))
  -- Assets other than present in the given minimal Value are not allowed.
  , selectUtxosStrict :: Value -> f (Maybe (Set.Set FullTxOut))
  }

mkPersistentWalletOutputs :: 
  ( MonadIO i
  , MonadIO f
  , MonadMask f
  , MonadResource i
  ) => (f ~> i) -> MakeLogging i f -> UtxoStoreConfig -> Explorer f -> Vault f -> i (WalletOutputs f)
mkPersistentWalletOutputs fToI mkLogging@MakeLogging{..} cfg explorer vaultF = do
  let Vault{..} = fmapK fToI vaultF
  logging <- forComponent "WalletOutputs"
  ustore  <- mkPersistentUtxoStoreWithCache mkLogging cfg
  pkh     <- getPaymentKeyHash
  fToI $ filterSpentedUtxos ustore explorer
  pure $ WalletOutputs
    { selectUtxos       = selectUtxos'' logging explorer ustore pkh False
    , selectUtxosStrict = selectUtxos'' logging explorer ustore pkh True
    }

filterSpentedUtxos :: (Monad f) => UtxoStore f -> Explorer f -> f ()
filterSpentedUtxos UtxoStore{..} Explorer{..} = do
  allUtxos <- getUtxos
  (\FullTxOut{..} -> getOutput fullTxOutRef >>= (\case 
      Just _  -> pure ()
      Nothing -> dropUtxos $ Set.fromList [fullTxOutRef]
    )) `traverse` toList allUtxos
  pure ()

mkWalletOutputs :: (MonadIO i, MonadIO f, MonadMask f) => MakeLogging i f -> Explorer f -> Hash PaymentKey -> i (WalletOutputs f)
mkWalletOutputs mkLogging@MakeLogging{..} explorer pkh = do
  logging <- forComponent "WalletOutputs"
  ustore  <- mkUtxoStore mkLogging
  pure $ WalletOutputs
    { selectUtxos       = selectUtxos'' logging explorer ustore pkh False
    , selectUtxosStrict = selectUtxos'' logging explorer ustore pkh True
    }

mkWalletOutputs' :: forall i f. (MonadIO i, MonadIO f, MonadMask f) => (f ~> i) -> MakeLogging i f -> Explorer f -> Vault f -> i (WalletOutputs f)
mkWalletOutputs' fToI mklogging explorer vaultF = do
  let Vault{..}  = fmapK fToI vaultF
  getPaymentKeyHash >>= mkWalletOutputs mklogging explorer

selectUtxos'' :: (MonadIO f, MonadMask f) => Logging f -> Explorer f -> UtxoStore f -> Hash PaymentKey -> Bool -> Value -> f (Maybe (Set.Set FullTxOut))
selectUtxos'' logging explorer ustore@UtxoStore{..} pkh strict requiredValue = do
  let
    fetchUtxos offset limit = do
      let
        paging  = Explorer.Paging offset limit
        mkPCred = Explorer.PaymentCred . T.decodeUtf8 . convertToBase Base16 . serialiseToRawBytes
      utxoBatch <- getUnspentOutputsByPCredWithRetry logging explorer (mkPCred pkh) paging
      putUtxos (Set.fromList $ Explorer.items utxoBatch <&> Explorer.toCardanoTx)
      let entriesLeft = Explorer.total utxoBatch - (offset + limit)

      if entriesLeft > 0
      then pure () -- fetchUtxos (offset + limit) limit
      else pure ()

    extractAssets v = Set.fromList (flattenValue v <&> (\(cs, tn, _) -> (cs, tn)))
    requiredAssets = extractAssets requiredValue

    collect :: [FullTxOut] -> Value -> [FullTxOut] -> Maybe [FullTxOut]
    collect acc valueAcc outs =
      case outs of
        fout@FullTxOut{..} : tl | valueAcc `lt` requiredValue ->
          if satisfies
          then collect (fout : acc) (fullTxOutValue <> valueAcc) tl
          else collect acc valueAcc tl -- current output doesn't contain the required asset at all, so skipping it
          where
            assets              = extractAssets fullTxOutValue
            containsTargetAsset = not $ Set.null $ Set.intersection assets requiredAssets
            containsOtherAssets = not $ Set.null $ Set.difference assets requiredAssets
            satisfies           = (containsTargetAsset && not strict) || (containsTargetAsset && not containsOtherAssets)
        [] | valueAcc `lt` requiredValue ->
          Nothing
        _ ->
          Just acc

  utxos <- getUtxos
  case collect [] mempty (Set.elems utxos) of
    Just outs -> pure $ Just $ Set.fromList outs
    Nothing   -> fetchUtxos 0 batchSize >> selectUtxos'' logging explorer ustore pkh strict requiredValue
      where batchSize = 400

getUnspentOutputsByPCredWithRetry :: (MonadIO f, MonadMask f) => Logging f -> Explorer f -> PaymentCred -> Paging -> f (Items Explorer.FullTxOut)
getUnspentOutputsByPCredWithRetry Logging{..} Explorer{..} cred paging = do
  let backoff = constantDelay 1000000
  recoverAll backoff (\rs -> infoM ("RetryStatus for getUnspentOutputsByPCredWithRetry " ++ (show rs)) >> (getUnspentOutputsByPCred cred paging))