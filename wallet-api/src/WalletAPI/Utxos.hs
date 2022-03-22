module WalletAPI.Utxos where

import           RIO
import qualified Data.Set                   as Set
import           Data.ByteArray.Encoding    (Base(..), convertToBase)
import qualified Data.Text.Encoding         as T


import           Ledger
import           Plutus.V1.Ledger.Value
import           Cardano.Api            hiding (Value)

import           CardanoTx.Models
import           WalletAPI.UtxoStore

import           Explorer.Service
import qualified Explorer.Types       as Explorer
import qualified Explorer.Models      as Explorer
import qualified Explorer.Class       as Explorer

data WalletOutputs f = WalletOutputs
  -- Select UTxOs satisfying the given minumal Value.
  { selectUtxos       :: Value -> f (Maybe (Set.Set FullTxOut))
  -- Assets other than present in the given minimal Value are not allowed.
  , selectUtxosStrict :: Value -> f (Maybe (Set.Set FullTxOut))
  }

mkWalletOutputs :: MonadIO f => Explorer f -> Hash PaymentKey -> f (WalletOutputs f)
mkWalletOutputs explorer pkh = do
  ustore <- mkUtxoStore
  pure $ WalletOutputs
    { selectUtxos       = selectUtxos'' explorer ustore pkh True
    , selectUtxosStrict = selectUtxos'' explorer ustore pkh False
    }

selectUtxos'' :: Monad f => Explorer f -> UtxoStore f -> Hash PaymentKey -> Bool -> Value -> f (Maybe (Set.Set FullTxOut))
selectUtxos'' explorer@Explorer{..} ustore@UtxoStore{..} pkh strict requiredValue = do
  let
    fetchUtxos offset limit = do
      let
        paging  = Explorer.Paging offset limit
        mkPCred = Explorer.PaymentCred . T.decodeUtf8 . convertToBase Base16 . serialiseToRawBytes
      utxoBatch <- getUnspentOutputsByPCred (mkPCred pkh) paging
      putUtxos (Set.fromList $ Explorer.items utxoBatch <&> Explorer.toCardanoTx)
      let entriesLeft = Explorer.total utxoBatch - (offset + limit)

      if entriesLeft > 0
      then fetchUtxos (offset + limit) limit
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
            containsOtherAssets = Set.null $ Set.difference assets requiredAssets
            satisfies           = (containsTargetAsset && not strict) || (containsTargetAsset && not containsOtherAssets)
        [] | valueAcc `lt` requiredValue ->
          Nothing
        _ ->
          Just acc

  utxos <- getUtxos
  case collect [] mempty (Set.elems utxos) of
    Just outs -> pure $ Just $ Set.fromList outs
    Nothing   -> fetchUtxos 0 batchSize >> selectUtxos'' explorer ustore pkh strict requiredValue
      where batchSize = 20
