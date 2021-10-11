module Cardano.PoolActions where

import           Cardano.Models
import           Cardano.Utils
import           ErgoDex.Contracts.Pool
import           ErgoDex.Contracts.Types
import           ErgoDex.OffChain
import           Plutus.V1.Ledger.Value
import qualified PlutusTx.AssocMap   as Map
import           Ledger.Scripts
import qualified Ledger.Typed.Scripts.Validators as Validators
import qualified PlutusTx
import           Data.List
import           Data.Maybe

createPool :: PoolParams -> ErgoTxOut -> ErgoTxOut -> ErgoTxOut -> TxCandidate
createPool pp@PoolParams{..} inputWithX inputWithY inputWithNft = let
  commonValue   = foldl (\acc curOutput -> acc <> (txOutValue curOutput)) (Value Map.empty) [inputWithX, inputWithY, inputWithNft]
  toInputs      = map fromPkTxOut2TxIn [inputWithX, inputWithY, inputWithNft]
  newPoolOutput = TxOutCandidate {
		address = Validators.validatorAddress poolInstance,
		value = commonValue,
		datum = Just $ Datum (PlutusTx.toBuiltinData pp)
  } in TxCandidate {
		inputs = toInputs,
		outputs = [newPoolOutput]
  }

initPool :: PoolParams -> PoolParams -> ErgoTxOut -> TxCandidate
initPool prevPoolParams newPoolParams prevPoolOut = let
  toInput = fromScriptTxOut2TxIn prevPoolOut
  newPoolOutput = TxOutCandidate {
    address = Validators.validatorAddress poolInstance,
		value = txOutValue prevPoolOut,
		datum = Just $ Datum (PlutusTx.toBuiltinData newPoolParams)
  }
  in TxCandidate {
		inputs = [toInput],
		outputs = [newPoolOutput]
  }