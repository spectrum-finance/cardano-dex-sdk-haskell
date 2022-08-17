module ErgoDex.Amm.Scripts where

import Control.Monad.IO.Class (MonadIO)
import Data.Functor           ((<&>))

import           Ledger
import qualified Plutus.V2.Ledger.Api           as PV2
import           Plutus.Script.Utils.V2.Address (mkValidatorAddress)

import ErgoDex.PValidators (poolValidator, swapValidator)

--todo: delete this file

poolScript :: (MonadIO m) => m PV2.Validator
poolScript = poolValidator 

poolAddress :: (MonadIO m) => m Address
poolAddress = poolScript <&> mkValidatorAddress

swapScript :: (MonadIO m) => m PV2.Validator
swapScript = swapValidator 

depositScript :: (MonadIO m) => m PV2.Validator
depositScript = depositScript

redeemScript :: (MonadIO m) => m PV2.Validator
redeemScript = redeemScript
