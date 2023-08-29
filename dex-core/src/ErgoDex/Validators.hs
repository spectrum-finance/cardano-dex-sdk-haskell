module ErgoDex.Validators
  ( Version(..)
  , PoolValidator(..)
  , OrderValidator(..)
  , orderValidator
  , type AnyOrderValidator
  , type SwapValidator
  , type DepositValidator
  , type RedeemValidator
  , type PoolValidatorV1
  , fetchPoolValidatorV1
  , fetchSwapValidatorV1
  , fetchDepositValidatorV1
  , fetchRedeemValidatorV1
  ) where

import Control.Monad.IO.Class (MonadIO)
import RIO ((<&>))

import qualified Plutus.V2.Ledger.Api as PV2

import ErgoDex.PValidators

newtype PoolValidator ver = PoolValidator PV2.Validator

data Version = V1 | V2

data SwapK
data DepositK
data RedeemK

data OrderValidator kind ver where
  SwapValidator    :: PV2.Validator -> OrderValidator SwapK ver
  DepositValidator :: PV2.Validator -> OrderValidator DepositK ver
  RedeemValidator  :: PV2.Validator -> OrderValidator RedeemK ver

type SwapValidator ver    = OrderValidator SwapK ver
type DepositValidator ver = OrderValidator DepositK ver
type RedeemValidator ver  = OrderValidator RedeemK ver

orderValidator :: OrderValidator kind ver -> PV2.Validator
orderValidator (SwapValidator sv)    = sv
orderValidator (DepositValidator dv) = dv
orderValidator (RedeemValidator rv)  = rv

type AnyOrderValidator ver = forall kind. OrderValidator kind ver

type PoolValidatorV1 = PoolValidator V1

fetchPoolValidatorV1 :: MonadIO m => m (PoolValidator V1) 
fetchPoolValidatorV1 = poolValidator <&> PoolValidator

fetchSwapValidatorV1 :: MonadIO m => m (SwapValidator V1)
fetchSwapValidatorV1 = swapValidator <&> SwapValidator

fetchDepositValidatorV1 :: MonadIO m => m (DepositValidator V1)
fetchDepositValidatorV1 = depositValidator <&> DepositValidator

fetchRedeemValidatorV1 :: MonadIO m => m (RedeemValidator V1)
fetchRedeemValidatorV1 = redeemValidator <&> RedeemValidator
