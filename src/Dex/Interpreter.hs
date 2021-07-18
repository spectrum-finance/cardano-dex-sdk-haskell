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
import qualified Data.Map                         as Map
import qualified Ledger.Typed.Scripts             as Scripts
import           Plutus.Contract                  hiding (when)
import           Proxy.Contract.Models
import           Proxy.Contract.OnChain
import qualified Data.Map                  as Map
import           Dex.Contract.OffChain
import           Dex.Instances
import           Wallet.Emulator.Wallet
import           Wallet.Effects                   (WalletEffect(..))
import           Wallet.API

data InterpreterService a b = InterpreterService
    { deposit :: (Operation SwapOpData) -> Pool -> Either MkTxError Tx
    , redeem :: (Operation DepositOpData) -> Pool -> Either MkTxError Tx
    , swap :: (Operation RedeemOpData) -> Pool -> Either MkTxError Tx
    }

mkInterpreterService :: InterpreterService a b
mkInterpreterService = InterpreterService deposit' redeem' swap'

--todo: lift MkTxError to dex error
interpretOp' :: Operation a -> Pool -> Either MkTxError (Tx, TxOut)
interpretOp' op pool =
    do
        tx <- createTx' op pool
        -- todo: use correct error
        newPoolOutput <- maybeToRight TypedValidatorMissing (getNewPoolOut' tx)
        let result = Right (tx, fullTxOut2TxOut newPoolOutput)
        result

--todo: lift MkTxError to dex error. Set correct errors. Now wip
createTx' :: (Operation a) -> Pool -> Either MkTxError Tx
createTx' operation pool
    | checkPool operation pool /= True = Left TypedValidatorMissing
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

deposit' :: Operation SwapOpData -> Pool -> Either MkTxError Tx
deposit' = createTx'

redeem' :: Operation DepositOpData -> Pool -> Either MkTxError Tx
redeem' = createTx'

swap' :: Operation RedeemOpData -> Pool -> Either MkTxError Tx
swap' = createTx'