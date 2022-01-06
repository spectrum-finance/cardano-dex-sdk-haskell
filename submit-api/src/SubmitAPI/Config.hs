module SubmitAPI.Config where

import qualified Dhall         as D
import           GHC.Generics

data FeePolicy
  = Strict  -- Require existing TX inputs to cover fee entirely
  | Balance -- Allow adding new inputs to cover fee
  deriving Generic

instance D.FromDhall FeePolicy

data CollateralPolicy
  = Ignore -- Ignore collateral inputs
  | Cover  -- Allow adding new inputs to cover collateral
  deriving Generic

instance D.FromDhall CollateralPolicy

data TxAssemblyConfig = TxAssemblyConfig
  { feePolicy        :: FeePolicy
  , collateralPolicy :: CollateralPolicy
  } deriving Generic

instance D.FromDhall TxAssemblyConfig
