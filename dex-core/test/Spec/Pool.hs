{-# LANGUAGE OverloadedStrings #-}

module Spec.Pool
  ( initialLiquidityTests
  , initPoolTests
  ) where

import qualified Data.ByteString as BS

import Test.Tasty
import Test.Tasty.HUnit

import Data.List
import Data.Ord

import PlutusTx.Builtins.Internal (BuiltinByteString(..))
import Plutus.V1.Ledger.Api       (CurrencySymbol(..), TokenName(..))
import Plutus.V1.Ledger.Value     (AssetClass(..))

import           ErgoDex.Amm.Pool
import qualified ErgoDex.Contracts.Typed as S
import           ErgoDex.Contracts.Types
import           ErgoDex.Types
import           ErgoDex.State
import           ErgoDex.Amm.PoolSetup   (burnLqInitial)
import           ErgoDex.Class           (ToLedger(toLedger))
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
  { poolId        = PoolId poolNft
  , poolReservesX = sufficientInitDepositX
  , poolReservesY = initDepositY
  , poolLiquidity = releasedLq
  , poolCoinX     = poolX
  , poolCoinY     = poolY
  , poolCoinLq    = poolLq
  , poolFee       = PoolFee poolFeeNum feeDen
  , outCollateral = minSafeOutputAmount
  }

initPoolTests = testGroup "NonNativePoolInit"
  [ testCase "init_non_native_pool_sufficient_liquidity" $
      initPool poolConf burnLqInitial (sufficientInitDepositX, initDepositY) @?= Right (Predicted (toLedger nativePool) nativePool, releasedLq)
  , testCase "init_non_native_pool_insufficient_liquidity" $
      initPool poolConf burnLqInitial (insufficientInitDepositX, initDepositY) @?= Left (InsufficientInitialLiqudity $ Amount 1000)
  ]
