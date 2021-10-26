module CardanoTx.TxAsm.Config where

data FeePolicy
  = Strict  -- Require existing TX inputs to cover fee entirely
  | Balance -- Allow adding new inputs to cover fee

data CollateralPolicy
  = Ignore -- Ignore collateral inputs
  | Cover  -- Allow adding new inputs to cover collateral

data AssemblyConfig = AssemblyConfig
  { feePolicy        :: FeePolicy
  , collateralPolicy :: CollateralPolicy
  }
