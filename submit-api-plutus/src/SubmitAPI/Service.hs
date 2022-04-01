module SubmitAPI.Service where

import RIO

import qualified Ledger           as P
import qualified CardanoTx.Models as Sdk

import           NetworkAPI.Service hiding (submitTx)

data Transactions f = Transactions
  { handleTx :: Sdk.TxCandidate  -> f P.CardanoTx
  }

mkSubmitService
  :: MonadThrow f
  => MonadIO f
  => Network f
  -> Transactions f
mkSubmitService network = Transactions
  { handleTx = handleTx' network
  }

handleTx'
  :: MonadThrow f
  => MonadIO f
  => Network f
  -> Sdk.TxCandidate
  -> f P.CardanoTx
handleTx' txc = undefined
  -- let utx = Tx.mkUnbalancedTx mempty txc
  -- in Eff.runM $ W.handleTx utx
