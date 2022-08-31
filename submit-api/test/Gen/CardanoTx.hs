{-# LANGUAGE OverloadedStrings #-}

module Gen.CardanoTx where

import           Data.Functor    ((<&>))
import qualified Data.ByteString as BS
import           Data.Map        as Map
import           Data.Set        as Set

import Hedgehog
import Hedgehog.Gen as Gen
import Hedgehog.Range as Range

import qualified Ledger.Ada                  as Ada
import qualified Ledger                      as P
import qualified Ledger.Value                as P
import qualified Ledger.Value                as Value
import qualified PlutusTx.Builtins.Internal  as P
import qualified Plutus.Script.Utils.Scripts as P
import qualified Plutus.V1.Ledger.Api        as P
import qualified Ledger.Interval             as Interval
import           Ledger.Ada                  (adaValueOf)
import qualified PlutusTx

import qualified CardanoTx.Models     as Sdk
import           CardanoTx.Models     (TxOutDatum(EmptyDatum))

mkTokenName :: BS.ByteString -> P.TokenName
mkTokenName = P.TokenName . P.BuiltinByteString

mkCurrencySymbol :: BS.ByteString -> P.CurrencySymbol
mkCurrencySymbol = P.CurrencySymbol . P.BuiltinByteString

mkAssetClass :: BS.ByteString -> BS.ByteString -> P.AssetClass
mkAssetClass cs tn = P.AssetClass (mkCurrencySymbol cs, mkTokenName tn)

mkValue :: P.AssetClass -> Integer -> P.Value
mkValue (P.AssetClass (cs, tn)) = Value.singleton cs tn

mkAdaValue :: Integer -> P.Value
mkAdaValue = mkValue (P.AssetClass (Ada.adaSymbol, Ada.adaToken))

genAdaValue :: MonadGen f => f P.Value
genAdaValue = integral (Range.constant (10^8) (10^12)) <&> mkAdaValue

genBuiltinByteString :: MonadGen f => Int -> f P.BuiltinByteString
genBuiltinByteString s = bytes (Range.singleton s) <&> P.BuiltinByteString

genTxId :: MonadGen f => f P.TxId
genTxId = prune $ genBuiltinByteString 32 <&> P.TxId

genTxOutRef :: MonadGen f => f P.TxOutRef
genTxOutRef = do
  txId <- genTxId
  ix   <- integral $ Range.constant 0 10
  pure $ P.TxOutRef txId ix

genPkh :: MonadGen f => f P.PubKeyHash
genPkh = genBuiltinByteString 28 <&> P.PubKeyHash

genPkhAddress :: MonadGen f => f P.Address
genPkhAddress = genPkh <&> (\pkh -> P.pubKeyHashAddress (P.PaymentPubKeyHash pkh) Nothing)

stablePkh :: P.PubKeyHash
stablePkh = "d74d26c5029cf290094fce1a0670da7369b9026571dfb977c6fa234f"

stableAddress :: P.Address
stableAddress = P.pubKeyHashAddress (P.PaymentPubKeyHash stablePkh) Nothing

data DummyDatum = DummyDatum Integer Bool
instance P.ToData DummyDatum where
  {-# INLINE toBuiltinData #-}
  toBuiltinData a = P.mkConstr 0 (P.mkCons (P.mkI 99) (P.mkCons (P.toBuiltinData True) (P.BuiltinList [])))

genNonEmptyDatum :: MonadGen f => f (P.Datum, P.DatumHash)
genNonEmptyDatum = Gen.constant (d, P.datumHash d)
  where d = P.Datum $ P.toBuiltinData $ DummyDatum 1 True

genFullTxOut :: MonadGen f => f Sdk.FullTxOut
genFullTxOut = do
  value <- genAdaValue
  genFullTxOutExact value

genFullTxOutExact :: MonadGen f => P.Value -> f Sdk.FullTxOut
genFullTxOutExact value = do
  ref   <- genTxOutRef
  addr  <- genPkhAddress
  pure $ Sdk.FullTxOut ref addr value EmptyDatum

genFullTxIn :: MonadGen f => f Sdk.FullTxIn
genFullTxIn = genFullTxOut <&> (`Sdk.FullTxIn` P.ConsumePublicKeyAddress)

genFullScriptTxIn :: MonadGen f => f Sdk.FullTxIn
genFullScriptTxIn = undefined

genFullTxInExact :: MonadGen f => P.Value -> f Sdk.FullTxIn
genFullTxInExact value = genFullTxOutExact value <&> (`Sdk.FullTxIn` P.ConsumePublicKeyAddress)

genTxOutCandidate :: MonadGen f => f Sdk.TxOutCandidate
genTxOutCandidate = do
  value <- genAdaValue
  genTxOutCandidateExact value

genTxOutCandidateExact :: MonadGen f => P.Value -> f Sdk.TxOutCandidate
genTxOutCandidateExact value = do
  addr <- genPkhAddress
  pure $ Sdk.TxOutCandidate addr value EmptyDatum

genPlainTxCandidate :: MonadGen f => f Sdk.TxCandidate
genPlainTxCandidate = do
  inputs  <- Gen.set (Range.constant 3 10) genFullTxIn <&> Set.elems
  outputs <- Gen.set (Range.constant 3 10) genTxOutCandidate <&> Set.elems
  let
    adaIn  = Prelude.foldl (\ acc i -> acc + (Ada.getLovelace $ Ada.fromValue $ Sdk.fullTxOutValue $ Sdk.fullTxInTxOut i)) 0 inputs
    adaOut = Prelude.foldl (\ acc i -> acc + (Ada.getLovelace $ Ada.fromValue $ Sdk.txOutCandidateValue i)) 0 outputs
    txFee  = 10^10
    delta  = adaOut + txFee - adaIn
  extraIn <- if delta > 0
    then genFullTxInExact (Ada.lovelaceValueOf delta) <&> pure
    else pure []
  let updatedInputs = Set.fromList $ inputs ++ extraIn
  pure $ Sdk.TxCandidate updatedInputs outputs mempty mempty Nothing Interval.always mempty
