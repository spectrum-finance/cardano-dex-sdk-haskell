module SubmitAPI.Service where

import RIO

import qualified CardanoTx.Models           as Sdk
import           Cardano.Api                as C
import qualified Plutus.Contract.CardanoAPI as Interop

import SubmitAPI.Config
import SubmitAPI.Internal.Transaction

data SubmitService f = SubmitService
  { finalizeTx :: Sdk.TxCandidate -> f (C.Tx C.AlonzoEra)
  , submitTx   :: C.Tx C.AlonzoEra -> f ()
  }

mkSubmitService :: TxAssemblyConfig -> SubmitService f
mkSubmitService TxAssemblyConfig{..} = undefined

finalizeTx'
  :: MonadThrow f
  => Sdk.TxCandidate
  -> f (C.Tx C.AlonzoEra)
finalizeTx' txc@(Sdk.TxCandidate{..}) = undefined