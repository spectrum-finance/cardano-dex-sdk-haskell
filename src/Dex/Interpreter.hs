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
import Plutus.V1.Ledger.Address 
import qualified PlutusTx.AssocMap                as MapValue
import Plutus.V1.Ledger.Value
import qualified PlutusTx
import Dex.Contract.Models
import PlutusTx.Data
import Plutus.V1.Ledger.Scripts
import Plutus.V1.Ledger.Credential
import Plutus.V1.Ledger.TxId
import PlutusTx.Builtins
import Plutus.V1.Ledger.Crypto

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
getNewPoolOut' tx = Just $ FullTxOut {
    refId = TxId "21fe31dfa154a261626bf854046fd2271b7bed4b6abe45aa58877ef47f9721b9",
    refIdx = 3,
    txOutAddress = Address {
        addressCredential = PubKeyCredential $ PubKeyHash "21fe31dfa154a261626bf854046fd2271b7bed4b6abe45aa58877ef47f9721b9",
        addressStakingCredential = Nothing
    },
    txOutValue = Value MapValue.empty,
    fullTxOutDatum = Datum ammDatumTestData
}

deposit' :: Operation SwapOpData -> Pool -> Either MkTxError Tx
deposit' = createTx'

redeem' :: Operation DepositOpData -> Pool -> Either MkTxError Tx
redeem' = createTx'

swap' :: Operation RedeemOpData -> Pool -> Either MkTxError Tx
swap' = createTx'

ergoDexPoolTest :: ErgoDexPool
ergoDexPoolTest = 
    ErgoDexPool {
        feeNum = 10,
        xCoin = AssetClass {
            unAssetClass = (
                CurrencySymbol {
                    unCurrencySymbol = emptyByteString
                }, 
                TokenName {
                    unTokenName = emptyByteString
                }
            )
        } ,
        yCoin = AssetClass {
            unAssetClass = (
                CurrencySymbol {
                    unCurrencySymbol = emptyByteString
                }, 
                TokenName {
                    unTokenName = emptyByteString
                }
            )
        }  ,
        lpCoin = AssetClass {
            unAssetClass = (
                CurrencySymbol {
                    unCurrencySymbol = emptyByteString
                }, 
                TokenName {
                    unTokenName = emptyByteString
                }
            )
        } 
    }

ammDatumTestData :: Data
ammDatumTestData = 
    PlutusTx.toData ergoDexPoolTest