module WalletAPI.Vault where

import           RIO
import qualified Data.Set                as Set
import qualified Data.ByteString         as BS
import           Data.ByteArray.Encoding (Base(..), convertToBase)
import qualified Data.Text               as T
import qualified Data.Text.Encoding      as T

import           Ledger
import           Plutus.V1.Ledger.Value
import           Cardano.Api hiding (Value)
import qualified Cardano.Api as Crypto

import CardanoTx.Models
import WalletAPI.TrustStore
import WalletAPI.UtxoStore
import Explorer.Service
import qualified Explorer.Types  as Explorer
import qualified Explorer.Models as Explorer
import qualified Explorer.Class  as Explorer

data Vault f = Vault
  { getSigningKey :: PubKeyHash -> f (Maybe ShelleyWitnessSigningKey)
  , selectUtxos   :: Value      -> f (Maybe (Set.Set FullTxOut))
  }

mkVault :: MonadIO f => Explorer f -> TrustStore f -> KeyPass -> f (Vault f)
mkVault explorer tstore pass = do
  ustore <- mkUtxoStore
  pure $ Vault
    { getSigningKey = getSigningKey' tstore pass
    , selectUtxos   = selectUtxos' explorer ustore tstore
    }

getSigningKey' :: Functor f => TrustStore f -> KeyPass -> PubKeyHash -> f (Maybe ShelleyWitnessSigningKey)
getSigningKey' TrustStore{..} pass _ = readSK pass <&> WitnessPaymentKey <&> Just

selectUtxos' :: Monad f => Explorer f -> UtxoStore f -> TrustStore f -> Value -> f (Maybe (Set.Set FullTxOut))
selectUtxos' explorer@Explorer{..} ustore@UtxoStore{..} tstore@TrustStore{..} requiredValue = do
  let
    fetchUtxos offset limit = do
      pkh <- readVK <&> Crypto.verificationKeyHash
      let paging = Explorer.Paging offset limit
      utxoBatch <- getUnspentOutputsByPCred (Explorer.PaymentCred $ base16 $ serialiseToRawBytes pkh) paging
      putUtxos (Set.fromList $ Explorer.items utxoBatch <&> Explorer.toCardanoTx)
      let entriesLeft = (Explorer.total utxoBatch) - (offset + limit)

      if entriesLeft > 0
      then fetchUtxos (offset + limit) limit
      else pure ()

    collect :: [FullTxOut] -> Value -> [FullTxOut] -> Maybe [FullTxOut]
    collect acc valueAcc outs =
      case outs of
        out@FullTxOut{..} : tl | valueAcc `lt` requiredValue ->
          if Set.null $ Set.intersection (extractAssets fullTxOutValue) (extractAssets requiredValue)
          then collect acc valueAcc tl -- current output doesn't contain the required asset at all, so skipping it
          else collect (out : acc) (fullTxOutValue <> valueAcc) tl -- need more outputs
          where
            extractAssets v = Set.fromList (flattenValue v <&> (\(cs, tn, _) -> (cs, tn)))
        [] | valueAcc `lt` requiredValue ->
          Nothing
        _ ->
          Just acc

  utxos <- getUtxos
  case collect [] mempty (Set.elems utxos) of
    Just outs -> pure $ Just $ Set.fromList outs
    Nothing   -> fetchUtxos 0 20 >> selectUtxos' explorer ustore tstore requiredValue

base16 :: BS.ByteString -> String
base16 = T.unpack . T.decodeUtf8 . convertToBase Base16
