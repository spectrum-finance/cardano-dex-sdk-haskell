module SubmitAPI.Service where

import qualified CardanoTx.Models as Sdk
import           Cardano.Api      as C

import SubmitAPI.Config

data SubmitService f = SubmitService
  { finalizeTx :: Sdk.TxCandidate -> f (C.Tx C.AlonzoEra)
  , submitTx   :: C.Tx C.AlonzoEra -> f ()
  }

mkSubmitService :: TxAssemblyConfig -> SubmitService f
mkSubmitService TxAssemblyConfig{..} = undefined

finalizeTx' :: Sdk.TxCandidate -> f (C.Tx C.AlonzoEra)
finalizeTx' Sdk.TxCandidate{..} = undefined

