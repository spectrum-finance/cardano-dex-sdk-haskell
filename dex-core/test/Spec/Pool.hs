{-# LANGUAGE OverloadedStrings #-}

module Spec.Pool
  ( initialLiquidityTests
  , initPoolTests
  , toFromLedgerPoolTests
  , checkDeposit
  , checkRedeem
  , checkSwap
  ) where

import qualified Data.ByteString as BS

import Control.Monad.IO.Class (MonadIO)

import Hedgehog

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.Hedgehog as HH

import Data.List
import Data.Ord

import PlutusTx.Builtins.Internal (BuiltinByteString(..))
import Plutus.V1.Ledger.Api       (CurrencySymbol(..), TokenName(..), toBuiltinData, TxOutRef(..))
import Plutus.V1.Ledger.Value     (AssetClass(..))

import           CardanoTx.Models        (TxOutCandidate(..), mkFullTxOut)
import           ErgoDex.Amm.Pool
import qualified ErgoDex.Contracts.Typed as S
import           ErgoDex.Contracts.Types
import           ErgoDex.Types
import           ErgoDex.State
import           ErgoDex.Validators
import           ErgoDex.Amm.PoolSetup   (burnLqInitial)
import           ErgoDex.Class           (ToLedger(toLedger), FromLedger(parseFromLedger))
import           ErgoDex.Amm.Constants   (minSafeOutputAmount)

mkTokenName :: BS.ByteString -> TokenName
mkTokenName = TokenName . BuiltinByteString

mkCurrencySymbol :: BS.ByteString -> CurrencySymbol
mkCurrencySymbol = CurrencySymbol . BuiltinByteString

mkAssetClass :: BS.ByteString -> BS.ByteString -> AssetClass
mkAssetClass cs tn = AssetClass (mkCurrencySymbol cs, mkTokenName tn)

poolNft :: Coin Nft
poolNft = Coin $ mkAssetClass "nft" "pool_nft"

poolX :: Coin X
poolX = Coin $ mkAssetClass "x" "pool_x"

poolY :: Coin Y
poolY = Coin $ mkAssetClass "y" "pool_y"

poolLq :: Coin Liquidity
poolLq = Coin $ mkAssetClass "lq" "pool_lq"

baseX :: Coin Base
baseX = Coin $ mkAssetClass "x" "pool_x"

baseY :: Coin Base
baseY = Coin $ mkAssetClass "y" "pool_y"

quoteX :: Coin Quote
quoteX = Coin $ mkAssetClass "x" "pool_x"

quoteY :: Coin Quote
quoteY = Coin $ mkAssetClass "y" "pool_y"

poolFeeNum = 995

initialLiquidityTests = testGroup "InitialLiquidity"
  [ testCase "initial_liquidity_exact" $
      initialLiquidityAmount poolLq (Amount 10, Amount 10) @?= Right (AssetAmount poolLq 10)

  , testCase "initial_liquidity_approximated" $
      initialLiquidityAmount poolLq (Amount 10, Amount 11) @?= Right (AssetAmount poolLq 11)
  ]

poolConf = S.PoolConfig poolNft poolX poolY poolLq poolFeeNum

sufficientInitDepositX = Amount 800

insufficientInitDepositX = Amount 500

initDepositY = Amount 2000

releasedLq = Amount 265

nativePool = Pool
  { poolId           = PoolId poolNft
  , poolReservesX    = sufficientInitDepositX
  , poolReservesY    = initDepositY
  , poolLiquidity    = releasedLq
  , poolCoinX        = poolX
  , poolCoinY        = poolY
  , poolCoinLq       = poolLq
  , poolFee          = PoolFee poolFeeNum feeDen
  , outCollateral    = minSafeOutputAmount
  , stakeAdminPolicy = []
  , lqBound          = Amount 0
  , stakeCred        = Nothing
  }

-- todo: remove me
initPoolTests = testGroup "NonNativePoolInit"
  [ 
  --   HH.testProperty "init_non_native_pool_sufficient_liquidity" initNonNativePoolSufficientLiquidity
  -- , HH.testProperty "init_non_native_pool_insufficient_liquidity" initNonNativePoolInsufficientLiquidity
  ]

initNonNativePoolInsufficientLiquidity :: Property
initNonNativePoolInsufficientLiquidity = property $ do
  pv <- fetchPoolValidatorV1
  let pool = initPool pv poolConf burnLqInitial (insufficientInitDepositX, initDepositY)
  pool === Left (InsufficientInitialLiqudity $ Amount 1000)

initNonNativePoolSufficientLiquidity :: Property
initNonNativePoolSufficientLiquidity = property $ do
  pv <- fetchPoolValidatorV1
  let
    res = initPool pv poolConf burnLqInitial (sufficientInitDepositX, initDepositY)
    nativePoolToLedger = toLedger pv nativePool
  res === Right (Predicted nativePoolToLedger nativePool, releasedLq)

fromLedgerPool :: MonadIO m => m (Maybe Pool)
fromLedgerPool = do
  pv <- fetchPoolValidatorV1
  let nativePoolToLedger = toLedger pv nativePool
  pure $ case parseFromLedger $ mkFullTxOut (TxOutRef "test" 1) nativePoolToLedger of
    Just (OnChain _ pool) -> Just pool
    _ -> Nothing

toFromLedgerPoolTests = testGroup "ToFromLedgerPoolTests"
  [  HH.testProperty "pool_before_to_ledger_and_after_from_ledger_is_eq" poolBeforeToLedgerAndAfterFromLedgerIsEq
  ]

poolBeforeToLedgerAndAfterFromLedgerIsEq :: Property
poolBeforeToLedgerAndAfterFromLedgerIsEq = property $ do
  fromLedger <- fromLedgerPool
  Just nativePool === fromLedger

depositedPP = nativePool
  { poolReservesX = Amount 880
  , poolReservesY = Amount 2200
  , poolLiquidity = Amount 291
  }

checkDeposit = testGroup "CheckDeposit"
  [ HH.testProperty  "correct_apply_deposit" correctApplyDeposit
  , testCase "lq_amount_is_correct" $
      liquidityAmount nativePool (Amount 80, Amount 200) @=? assetAmountCoinOf poolLq 26
  ]

correctApplyDeposit :: Property
correctApplyDeposit = property $ do
  pv <- fetchPoolValidatorV1
  let
    deposit = applyDeposit pv nativePool (Amount 80, Amount 200)
    depositedPPToLedger = toLedger pv depositedPP
  deposit === Predicted depositedPPToLedger depositedPP

redeemedPP = nativePool
  { poolReservesX = Amount 802
  , poolReservesY = Amount 2004
  , poolLiquidity = Amount 265
  }

checkRedeem = testGroup "CheckRedeem"
  [ testCase "share_amount_is_correct" $
      sharesAmount depositedPP (Amount 26) @=? (assetAmountCoinOf poolX 78, assetAmountCoinOf poolY 196) -- losts some tokens due to reward lq approximate calculation 
  , HH.testProperty "correct_apply_redeem" $
      correctApplyRedeem -- losts some tokens due to reward lq approximate calculation 
  ]

correctApplyRedeem :: Property
correctApplyRedeem = property $ do
  pv <- fetchPoolValidatorV1
  let
    redeem = applyRedeem pv depositedPP (Amount 26)
    redeemPPToLedger = toLedger pv redeemedPP
  redeem === Predicted redeemPPToLedger redeemedPP

swapXPP = nativePool
  { poolReservesX = Amount 820
  , poolReservesY = Amount 1952
  , poolLiquidity = Amount 265
  }

swapYPP = nativePool
  { poolReservesX = Amount 770
  , poolReservesY = Amount 2080
  , poolLiquidity = Amount 265
  }

checkSwap = testGroup "SwapCheck"
  [ testCase "correct_output_amount_x_base" $
      outputAmount nativePool (assetAmountCoinOf baseX 20) @=? assetAmountCoinOf quoteY 48
  , testCase "correct_output_amount_y_base" $
      outputAmount nativePool (assetAmountCoinOf baseY 80) @=? assetAmountCoinOf quoteX 30
  , HH.testProperty "correct_apply_swap_x_base" correctApplySwapXBase
  , HH.testProperty "correct_apply_swap_y_base" correctApplySwapYBase
  ]

correctApplySwapXBase :: Property
correctApplySwapXBase = property $ do
  pv <- fetchPoolValidatorV1
  let
    swap = applySwap pv nativePool (assetAmountCoinOf baseX 20)
    swapXPPToLedger = toLedger pv swapXPP
  swap === Predicted swapXPPToLedger swapXPP

correctApplySwapYBase :: Property
correctApplySwapYBase = property $ do
  pv <- fetchPoolValidatorV1
  let
    swap = applySwap pv nativePool (assetAmountCoinOf baseY 80)
    swapYPPToLedger = toLedger pv swapYPP
  swap === Predicted swapYPPToLedger swapYPP
