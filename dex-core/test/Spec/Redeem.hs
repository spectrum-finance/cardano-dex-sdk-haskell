{-# LANGUAGE OverloadedStrings #-}

module Spec.Redeem where

import Plutus.V1.Ledger.Api
import Plutus.V1.Ledger.Value as Value
import qualified Ledger.Ada   as Ada
import qualified Data.Text    as T
import PlutusTx.Builtins.Internal

import qualified ErgoDex.Contracts.Proxy.Redeem as R

defaultCs :: CurrencySymbol
defaultCs = CurrencySymbol $ BuiltinByteString $ mkByteString $ T.pack "805fe1efcdea11f1e959eff4f422f118aa76dca2d0d797d184e487da"

lqTokenName :: TokenName
lqTokenName = TokenName $ BuiltinByteString $ mkByteString $ T.pack "6572676f6c6162736c70746f6b656e"

xTokenName :: TokenName
xTokenName = TokenName $ BuiltinByteString $ mkByteString $ T.pack "415f546f6b656e5f6e65775f706f6f6c0a"

yTokenName :: TokenName
yTokenName =TokenName $ BuiltinByteString $  mkByteString $ T.pack "425f546f6b656e5f6e65775f706f6f6c0a"

nftTokenName :: TokenName
nftTokenName = TokenName $ BuiltinByteString $ mkByteString $ T.pack "56594649745F414441745F6C70"

redeemConfig =
  R.RedeemConfig
    (AssetClass (defaultCs, nftTokenName))
    (AssetClass (defaultCs, xTokenName))
    (AssetClass (defaultCs, yTokenName))
    (AssetClass (defaultCs, lqTokenName))
    50000

mkRedeemConfig :: AssetClass -> AssetClass -> AssetClass -> AssetClass -> Integer -> PubKeyHash -> R.RedeemConfig
mkRedeemConfig x y lq nft fee pkh =
  R.RedeemConfig nft x y lq fee pkh Nothing

genRConfig :: AssetClass -> AssetClass -> AssetClass -> AssetClass -> Integer -> PubKeyHash -> (Data, DatumHash)
genRConfig x y lq nft fee pkh =
  let 
    config = mkRedeemConfig x y lq nft fee pkh
    dh     = mkDatumHash $ mkDatum config
  in (toData config, dh)

confirmedRedeemFullTxOut =
  FullTxOut
    { fullTxOutRef = TxOutRef ("57e339d380597e355fdf8e626cc7be4f56389f31ffdce763436899f564617462", 1)
    , fullTxOutAddress = ???
    , fullTxOutValue = Value.singleton (defaultCs, lqTokenName, 10) <> Value.singleton (Ada.adaSymbol, Ada.adaToken, 2500000)
    , fullTxOutDatum = ???
    }