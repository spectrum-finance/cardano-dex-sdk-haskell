{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -ddump-splices #-}

module Spectrum.EventSource.Data.TxEvent
  ( TxEvent(..)
  ) where

import Data.Aeson
import Data.Aeson.GADT.TH

import Spectrum.EventSource.Data.Tx
  ( MinimalTx )
import Spectrum.EventSource.Data.TxContext
  ( TxCtx(MempoolCtx, LedgerCtx) )
import qualified Ledger as P
import Cardano.Api (ToJSON)

data TxEvent ctx where
  PendingTx   :: MinimalTx 'MempoolCtx -> TxEvent 'MempoolCtx
  AppliedTx   :: MinimalTx 'LedgerCtx  -> TxEvent 'LedgerCtx
  UnappliedTx :: P.TxId -> TxEvent 'LedgerCtx

deriving instance Eq (TxEvent ctx)
deriving instance Show (TxEvent ctx)

deriveJSONGADT ''TxEvent