{-# LANGUAGE OverloadedStrings #-}

module Spec.Redeem where

import Plutus.V1.Ledger.Api
import Plutus.V1.Ledger.Value as Value
import qualified Ledger.Ada   as Ada
import qualified Data.Text    as T
import PlutusTx.Builtins.Internal

import qualified ErgoDex.Contracts.Proxy.Redeem as R

address = Address
  { addressCredential = PubKeyCredential "addr_test1vpdpn2w843atyy8h6evz3xll5wpsctyujzuewfg9x8qvurq2x2g93"
  , addressStakingCredential = Nothing
  }

stablePkh :: P.PubKeyHash
stablePkh = "d74d26c5029cf290094fce1a0670da7369b9026571dfb977c6fa234f"

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

coinNft :: Coin Nft
coinNft = Coint $ AssetClass (defaultCs, nftTokenName)

coinLq :: Coint Liquidity
cointLq = Coin $ (defaultCs, lqTokenName)

amountLq :: Amount Liquidity
amountLq = Amount 10

redeemConfig =
  R.RedeemConfig
    (AssetClass (defaultCs, nftTokenName))
    (AssetClass (defaultCs, xTokenName))
    (AssetClass (defaultCs, yTokenName))
    (AssetClass (defaultCs, lqTokenName))
    50000

redeemDatum = mkRedeemConfig x y lq nft fee pkh

mkRedeemConfig :: AssetClass -> AssetClass -> AssetClass -> AssetClass -> Integer -> PubKeyHash -> R.RedeemConfig
mkRedeemConfig x y lq nft fee pkh =
  R.RedeemConfig nft x y lq fee pkh Nothing

genRConfig :: AssetClass -> AssetClass -> AssetClass -> AssetClass -> Integer -> PubKeyHash -> (Data, DatumHash)
genRConfig x y lq nft fee pkh =
  let 
    config = mkRedeemConfig x y lq nft fee pkh
    dh     = mkDatumHash $ mkDatum config
  in (toData config, dh)

redeemOrder = 
  Redeem
    { redeemPoolId = PoolId $ coinNft
    , redeemLqIn = amountLq
    , redeemLq = coinLq
    , redeemExFee = ExFee 10
    , redeemRewardPkh = stablePkh
    , redeemRewardSPkh = Nothing
    }

confirmedRedeem = Confirmed confirmedRedeemFullTxOut redeemOrder

confirmedRedeemFullTxOut =
  FullTxOut
    { fullTxOutRef     = TxOutRef ("57e339d380597e355fdf8e626cc7be4f56389f31ffdce763436899f564617462", 1)
    , fullTxOutAddress = address
    , fullTxOutValue   = Value.singleton (defaultCs, lqTokenName, 10) <> Value.singleton (Ada.adaSymbol, Ada.adaToken, 2500000)
    , fullTxOutDatum   = KnownDatum $ mkDatum redeemDatum
    }