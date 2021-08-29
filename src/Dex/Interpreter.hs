{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE GADTs                 #-}

module Dex.Interpreter
    ( InterpreterService(..)
    , mkInterpreterService
    ) where

import           Control.Monad.Freer
import           Dex.Models
import           Dex.Utils
import           Plutus.V1.Ledger.Tx
import           Plutus.V1.Ledger.Scripts         (Redeemer(..))
import           Ledger.Constraints               as Constraints
import qualified PlutusTx.Builtins                as Builtins
import qualified Data.Set                         as Set
import           Data.Either.Combinators          (maybeToRight)
import           Dex.Utils
import           PlutusTx.IsData
import qualified PlutusTx
import           Data.Either.Combinators
import qualified Data.Map                         as Map
import qualified Ledger.Typed.Scripts             as Scripts
import           Plutus.Contract                  hiding (when)
import           Proxy.Contract.Models
import           Proxy.Contract.OnChain
import qualified Data.Map                  as Map
import           Dex.Contract.OnChain
import           Dex.Instances
import           Wallet.Emulator.Wallet
import           Wallet.Effects                   (WalletEffect(..))
import           Wallet.API

-- InterpreterService produce tx by interpreting Operation with some data with corresponding pool
data InterpreterService = InterpreterService
    { swap :: (Operation SwapOpData) -> Pool -> Either ProcError Tx
    , deposit :: (Operation DepositOpData) -> Pool -> Either ProcError Tx
    , redeem :: (Operation RedeemOpData) -> Pool -> Either ProcError Tx
    }

mkInterpreterService :: InterpreterService
mkInterpreterService = InterpreterService swap' deposit' redeem'

interpretOp' :: Operation a -> Pool -> Either ProcError (Tx, TxOut)
interpretOp' op pool =
    do
        tx <- createTx' op pool
        newPoolOutput <- maybeToRight (OutputWithPoolGenerationFailed "OutputWithPoolGenerationFailed") (getNewPoolOut' tx)
        let result = Right (tx, fullTxOut2TxOut newPoolOutput)
        result

createTx' :: (Operation a) -> Pool -> Either ProcError Tx
createTx' operation pool
    | checkPool operation pool /= True = Left (IncorrectPool ("Incorrect pool" ++ (show pool)))
    | otherwise = let
        inputs = getInputs operation pool
        outputs = generateOutputs operation pool
    --todo: remove generateEmptyValue and empty sets after tests
    in Right (
        Tx {
            txInputs = inputs,
            txCollateral = Set.empty,
            txOutputs = outputs,
            txForge = generateEmptyValue,
            txFee = generateEmptyValue,
            txValidRange = defaultSlotRange,
            txForgeScripts = Set.empty,
            txSignatures = Map.empty,
            txData = Map.empty
        }
    )

getNewPoolOut' :: Tx -> Maybe FullTxOut
getNewPoolOut' tx = undefined

swap' :: Operation SwapOpData -> Pool -> Either ProcError Tx
deposit' = createTx'

deposit' :: Operation DepositOpData -> Pool -> Either ProcError Tx
redeem' = createTx'

redeem' :: Operation RedeemOpData -> Pool -> Either ProcError Tx
swap' = createTx'