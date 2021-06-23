{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE Rank2Types            #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# OPTIONS_GHC -Wno-orphans  #-}

module Api.Backend.Test where

import           Control.Monad.Freer
import           Control.Monad.IO.Class
import           Ledger.Constraints.TxConstraints as Constraints
import           Control.Monad.Freer.State
import           Control.Monad.Freer.Extras.Log (LogMsg, logInfo)
import           Wallet.Effects
import           Wallet.Emulator.LogMessages    (RequestHandlerLogMsg, TxBalanceMsg (..))
import           Ledger.Constraints.OffChain
import           Wallet.Emulator.Wallet         (Wallet(..), walletPrivKey, WalletState (WalletState), emptyWalletState, PaymentArgs (ownPubKey))
import qualified Ledger.Generators              as Gen
import           Ledger                         (Slot, Block, PrivateKey)
import           Wallet.Emulator.NodeClient     (NodeClientState, emptyNodeClientState)
import qualified PlutusTx

createEffStr :: Eff '[] String
createEffStr = pure "41"

data TestData a where
    Test :: Int -> TestData Int
    ConsolePutStr :: Int -> TestData ()

addMember :: Member TestData effs => Int -> Eff effs ()
addMember = send . ConsolePutStr

-- genWalletEffect :: WalletEffect a
-- genWalletEffect = BalanceTx

effWithNodeClientEffect :: Member NodeClientEffect effs => NodeClientEffect a -> Eff effs a
effWithNodeClientEffect = send

effWithChainIndexEffect :: Member ChainIndexEffect effs => ChainIndexEffect [Block] -> Eff effs [Block]
effWithChainIndexEffect = send

addBalanceEffect :: Member WalletEffect effs => WalletEffect a -> Eff effs a
addBalanceEffect = send

effWithState :: Member (State WalletState) effs => WalletState -> Eff effs ()
effWithState = put

addLogging :: Member (LogMsg TxBalanceMsg) effs => TxBalanceMsg -> Eff effs ()
addLogging = logInfo

walletInst :: Wallet
walletInst = Wallet 1

privKey :: PrivateKey
privKey = walletPrivKey walletInst

genWalletState :: WalletState
genWalletState = emptyWalletState walletInst

runConsole :: Eff '[TestData, IO] a -> IO a
runConsole = runM . interpretM (\case
        ConsolePutStr msg -> print msg
    )