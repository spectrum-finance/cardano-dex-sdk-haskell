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
    { interpretOp :: Operation a -> Pool -> Either MkTxError (Tx, TxOut)
    , createTx :: (Operation b) -> Pool -> Either MkTxError Tx
    , getNewPoolOut :: Tx -> Maybe FullTxOut
    }

mkInterpreterService :: InterpreterService a b
mkInterpreterService = InterpreterService interpretOp' createTx' getNewPoolOut'

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

-- createSwapTransaction proxyTxOutRef proxyDatum datum o =
    -- let
    --     value = lovelaceValueOf 10
    --     lookups  = Constraints.otherData datum <>
    --                Constraints.otherScript (Scripts.validatorScript proxyInstance) <>
    --                Constraints.unspentOutputs (Map.singleton proxyTxOutRef o)

    --     redeemer = Redeemer $ PlutusTx.toData Swap

    --     tx =  Constraints.mustSpendScriptOutput proxyTxOutRef redeemer <>
    --           Constraints.mustPayToTheScript proxyDatum value

    --     unTx = Constraints.mkTx @ProxySwapping lookups tx
    -- in unTx
getNewPoolOut' :: Tx -> Maybe FullTxOut
getNewPoolOut' _ = undefined