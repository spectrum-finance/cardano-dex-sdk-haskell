{-# LANGUAGE TypeOperators          #-}
module Tests where

import Plutarch.Api.V1
import PExtra.API
import Plutarch.Prelude
import PExtra.Monadic
import Plutarch

import qualified ErgoDex.Contracts.Pool as Pool
import qualified ErgoDex.PContracts.PPool as PPool

import Models.PGenerator
import Models.Generator
import Models.Utils

runSuccessDeposite :: Term s PUnit
runSuccessDeposite = phoistAcyclic $ unTermCont $ do
  let
    cs    = genCurrencySymbol genCS
    nftTn = genTokenName genNft
    xTn   = genTokenName genX
    yTn   = genTokenName genY
    lqTn  = genTokenName genLQ


    pnft = pgenAssetClass nftTn cs
    px   = pgenAssetClass xTn cs
    py   = pgenAssetClass yTn cs
    plq  = pgenAssetClass lqTn cs


    nft = genAssetClass cs nftTn
    x   = genAssetClass cs xTn
    y   = genAssetClass cs yTn
    lq  = genAssetClass cs lqTn

    poolConfig  = genPoolConfig nft x y lq 100
    ppoolConfig = pconstant poolConfig
    poolDatum   = genDatum poolConfig
    poolDH      = genDatumHash poolDatum

    orderConfig  = genDepositConfig nft x y lq 100 pubKeyHashReward 100
    -- pOrderConfig = pconstant orderConfig
    orderDatum   = genOrderDatum orderConfig
    orderDH      = genDatumHash orderDatum

    poolInValue  = genValues [genValue nft 1, genValue x 10, genValue y 10, genValue lq (genMaxLq - 10), genAdaValue 1000000] mempty
    orderInValue = genValues [genValue x 10, genValue y 10, genAdaValue 1000000] mempty

    poolOutValue  = genValues [genValue nft 1, genValue x 20, genValue y 20, genValue lq (genMaxLq - 20), genAdaValue 1000000] mempty
    orderOutValue = genValues [genValue lq 10, genAdaValue (1000000 - 300)] mempty 

    poolInOut  = pgenPoolOut poolDH poolInValue pgenPoolValidator
    orderInOut = pgenPoolOut orderDH orderInValue pgenDepositValidator
    poolInIn   = pgenPoolIn genTxOutRef poolInOut
    orderInIn  = pgenPoolIn genTxOutRef orderInOut

    poolOut  = pgenPoolOut poolDH poolOutValue pgenPoolValidator
    orderOut = pgenPoolOut orderDH orderOutValue pgenDepositValidator

    txInfo  = pgenTxInfo poolInIn orderInIn poolOut orderOut
    purpose = pgenPurpose genTxOutRef
    cxt     = pgenContext txInfo purpose

    poolRedeemer = pgenPoolRedeemer # pcon PPool.Deposit # 0

    runPool = PPool.poolValidatorT # ppoolConfig # poolRedeemer # cxt

    res = pif (runPool) (pcon PUnit) perror
  return res