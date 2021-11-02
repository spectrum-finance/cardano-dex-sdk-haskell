module SubmitAPI.Service where

import RIO

import qualified CardanoTx.Models           as Sdk
import           Cardano.Api                as C
import qualified Plutus.Contract.CardanoAPI as Interop

import SubmitAPI.Config
import SubmitAPI.Internal.Transaction
import NetworkAPI.Service
import NetworkAPI.Env
import WalletAPI.Vault

data SubmitService f = SubmitService
  { finalizeTx :: Sdk.TxCandidate -> f (C.Tx C.AlonzoEra)
  , submitTx   :: C.Tx C.AlonzoEra -> f ()
  }

mkSubmitService :: TxAssemblyConfig -> SubmitService f
mkSubmitService TxAssemblyConfig{..} = undefined

finalizeTx'
  :: MonadThrow f
  => NetworkParams f
  -> Vault f
  -> Sdk.TxCandidate
  -> f (C.Tx C.AlonzoEra)
finalizeTx' network wallet txc@(Sdk.TxCandidate{..}) = undefined