module SubmitAPI.Config where

data FeePolicy
  = Strict  -- Require existing TX inputs to cover fee entirely
  | Balance -- Allow adding new inputs to cover fee

data CollateralPolicy
  = Ignore -- Ignore collateral inputs
  | Cover  -- Allow adding new inputs to cover collateral

data TxAssemblyConfig = TxAssemblyConfig
  { feePolicy        :: FeePolicy
  , collateralPolicy :: CollateralPolicy
  }
