{-# LANGUAGE OverloadedStrings          #-}

module Models.Generator
  ( genTxOutRef
  , genTokenName
  , genCurrencySymbol
  , genAssetClass
  , genValue
  , genAdaValue
  , genPoolConfig
  , genDatum
  , genDatumHash
  , genPoolRedeemer
  , genRedeemer
  , genMaxLq
  , genPoolValue
  , genTxInType
  , genScriptCredential
  , genFullTxOut
  , genTxIn
  , getTxOutCandidate
  , pPubKeyHashReward
  , genTxCandidate
  ) where

import Models.Utils (genByteString, unsafeFromEither)

import qualified ErgoDex.Contracts.Pool as P
import ErgoDex.Amm.Scripts ()
import qualified ErgoDex.Amm.PScripts as PScript
import CardanoTx.Models

import Plutus.V1.Ledger.Api
import Plutus.V1.Ledger.Value (AssetClass(..))
import PlutusTx.Builtins.Internal (BuiltinByteString(..))
import PlutusTx.AssocMap as Map
import qualified Ledger.Ada      as Ada
import qualified Ledger as Ledger
import qualified Ledger.Typed.Scripts.Validators as LV
import           Plutus.V1.Ledger.Credential (Credential (..))
import qualified PlutusTx
import qualified Ledger.Interval as Interval

import qualified Data.Set as Set
import GHC.Generics

genTxOutRef :: Integer -> TxOutRef
genTxOutRef index = TxOutRef (TxId (BuiltinByteString genByteString)) index

genTokenName :: TokenName
genTokenName = TokenName . BuiltinByteString $ genByteString

genCurrencySymbol :: CurrencySymbol
genCurrencySymbol = CurrencySymbol . BuiltinByteString $ genByteString

genAssetClass :: CurrencySymbol -> TokenName -> AssetClass
genAssetClass cs tn = AssetClass (cs, tn)

genValue :: AssetClass -> Integer -> Value
genValue (AssetClass (cs, tn)) qty = Value $ Map.fromList [(cs, Map.singleton tn qty)]

genAdaValue :: Integer -> Value
genAdaValue qty = genValue (genAssetClass Ada.adaSymbol Ada.adaToken) qty

genValues :: [Value] -> Value -> Value
genValues (x:xs) acc = genValues xs (x <> acc)
genValues [] acc = acc

genPoolConfig :: AssetClass -> AssetClass -> AssetClass -> AssetClass -> Integer -> P.PoolConfig
genPoolConfig nft x y lq fee = P.PoolConfig nft x y lq fee

genDatum :: P.PoolConfig -> Datum
genDatum = Datum . toBuiltinData

genDatumHash :: Datum -> DatumHash
genDatumHash datum = Ledger.datumHash datum

genPoolRedeemer :: Integer -> P.PoolAction -> P.PoolRedeemer
genPoolRedeemer ix action = P.PoolRedeemer action ix

genRedeemer :: P.PoolRedeemer -> Redeemer
genRedeemer = Redeemer . toBuiltinData

genMaxLq :: Integer
genMaxLq = 0x7fffffffffffffff

genPoolValue :: Value
genPoolValue =
  let
    cs = genCurrencySymbol

    nft = genAssetClass cs genTokenName
    x   = genAssetClass cs genTokenName
    y   = genAssetClass cs genTokenName
    lq  = genAssetClass cs genTokenName

    nftV = genValue nft 1
    xV   = genValue x 100
    yV   = genValue y 100
    lqV  = genValue lq genMaxLq
    adaV = genAdaValue 100000

  in genValues [nftV, xV, yV, lqV, adaV] mempty

genTxInType :: Datum -> Redeemer -> Ledger.TxInType
genTxInType datum redeemer = Ledger.ConsumeScriptAddress PScript.poolValidator redeemer datum 

genScriptCredential :: Credential
genScriptCredential = ScriptCredential $ LV.validatorHash $ LV.unsafeMkTypedValidator $ PScript.poolValidator

genFullTxOut :: TxOutRef -> Value -> Datum -> FullTxOut
genFullTxOut ref value datum =
  let
    address   = Address genScriptCredential Nothing
    datumHash = genDatumHash datum
  in
    FullTxOut
      { fullTxOutRef = ref
      , fullTxOutAddress = address
      , fullTxOutValue = value
      , fullTxOutDatumHash = Just datumHash
      , fullTxOutDatum = Just datum
      }

genTxCandidate :: TxCandidate
genTxCandidate =
  let
    cs = genCurrencySymbol

    nft = genAssetClass cs genTokenName
    x   = genAssetClass cs genTokenName
    y   = genAssetClass cs genTokenName
    lq  = genAssetClass cs genTokenName

    nftV = genValue nft 1
    xV   = genValue x 100
    yV   = genValue y 100
    lqV  = genValue lq genMaxLq
    adaV = genAdaValue 100000

    values = genValues [nftV, xV, yV, lqV, adaV] mempty

    poolCfg = genPoolConfig nft x y lq 100

    poolRedeemer = genPoolRedeemer 0 P.Swap

    datum = genDatum poolCfg

    redeemer = genRedeemer poolRedeemer

    ref = genTxOutRef 0

    txOut = genFullTxOut ref values datum

    txIn = genTxIn txOut datum redeemer

    xVSwap = genValue x (-10)
    yVSwap = genValue y 10
    lqVSwap = genValue lq (-100)

    swapValues = genValues [xVSwap, yVSwap, lqVSwap] values

    txOutCandidate = getTxOutCandidate swapValues datum
  in 
    TxCandidate 
      { txCandidateInputs = Set.fromList [txIn],
        txCandidateOutputs = [txOutCandidate],
        txCandidateValueMint = mempty,
        txCandidateMintInputs = mempty,
        txCandidateChangePolicy = Just $ ReturnTo (Address (PubKeyCredential pPubKeyHashReward) Nothing),
        txCandidateValidRange   = Interval.always,
        txCandidateSigners = []
      } 

genTxIn :: FullTxOut -> Datum -> Redeemer -> FullTxIn
genTxIn txOut datum redeemer =
  let
    txInType = genTxInType datum redeemer
  in
    FullTxIn
      { fullTxInTxOut = txOut
      , fullTxInType  = txInType
      }

getTxOutCandidate :: Value -> Datum -> TxOutCandidate
getTxOutCandidate value datum =
  let
    address = Address genScriptCredential Nothing
  in
    TxOutCandidate
        { txOutCandidateAddress = address
        , txOutCandidateValue   = value
        , txOutCandidateDatum   = Just datum
        }

pPubKeyHashReward :: PubKeyHash
pPubKeyHashReward = "d74d26c5029cf290094fce1a0670da7369b9026571dfb977c6fa234f"