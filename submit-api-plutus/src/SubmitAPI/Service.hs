module SubmitAPI.Service where

import Control.Monad.Freer as Eff

import qualified RIO.List as L
import           RIO
import qualified Data.Set              as Set
import qualified Data.ByteString.Char8 as B8
import           GHC.Natural           (naturalToInteger)

import qualified PlutusTx.AssocMap           as Map
import qualified Cardano.Api                 as C
import qualified Ledger                      as P
import qualified PlutusTx.Builtins.Internal  as P
import qualified Ledger.Ada                  as P
import qualified Plutus.V1.Ledger.Credential as P
import           Plutus.V1.Ledger.Api        (Value(..))
import qualified Plutus.Contract.Wallet      as W

import qualified CardanoTx.Models               as Sdk
import           SubmitAPI.Internal.Transaction as Tx
import           NetworkAPI.Service             hiding (submitTx)
import qualified NetworkAPI.Service             as Network
import           NetworkAPI.Env
import           WalletAPI.Utxos
import           WalletAPI.Vault

data Transactions f = Transactions
  { handleTx :: Sdk.TxCandidate  -> f P.CardanoTx
  }

mkSubmitService
  :: MonadThrow f
  => MonadIO f
  => Network f
  -> Transactions f
mkSubmitService = Transactions
  { handleTx = handleTx'
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
