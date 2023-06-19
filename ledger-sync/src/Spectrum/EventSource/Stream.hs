{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RecordWildCards #-}
module Spectrum.EventSource.Stream
  ( EventSource(..)
  , mkLedgerEventSource
  , mkMempoolTxEventSource
  ) where

import RIO
  ( (&), MonadReader, (<&>), fromMaybe, ($>) )

import Data.ByteString.Short
  ( toShort )

import Ledger
  ( TxId )

import qualified Data.Foldable as Foldable

import Control.Monad.IO.Class
  ( MonadIO )
import Control.Monad.Catch
  ( MonadThrow )
import Control.Monad
  ( join )
import Data.Sequence.Strict

import Streamly.Prelude as S

import System.Logging.Hlog
  ( MakeLogging(..), Logging(..) )

import qualified Ouroboros.Consensus.Protocol.Praos.Header as Praos
import Ouroboros.Consensus.Shelley.Ledger
  ( ShelleyBlock(ShelleyBlock), ShelleyHash (unShelleyHash), GenTx (..) )
import Ouroboros.Consensus.HardFork.Combinator
  ( OneEraHash(OneEraHash) )
import Ouroboros.Consensus.Cardano.Block
  ( HardForkBlock(BlockBabbage), GenTx (GenTxBabbage) )
import Ouroboros.Consensus.Block
  ( Point )

import Spectrum.EventSource.Data.Tx (MinimalTx(..), MinimalConfirmedTx (..))

import Cardano.Ledger.Alonzo.TxSeq
  ( TxSeq(txSeqTxns) )
import qualified Cardano.Ledger.Block as Ledger
import qualified Cardano.Crypto.Hash as CC
import Data.List

import Spectrum.LedgerSync.Protocol.Client
  ( Block )
import Spectrum.EventSource.Data.Tx
  ( fromBabbageLedgerTx, fromMempoolBabbageLedgerTx )
import Spectrum.LedgerSync
  ( LedgerSync(..) )
import Spectrum.Prelude.Context
  ( HasType, askContext )
import Spectrum.Config
  ( EventSourceConfig (EventSourceConfig, startAt) )
import Spectrum.EventSource.Types
  ( ConcretePoint (ConcretePoint)
  , toPoint
  , fromPoint
  , ConcretePoint (slot)
  , ConcreteHash (ConcreteHash)
  )
import Spectrum.EventSource.Persistence.LedgerHistory
  ( LedgerHistory (..), mkLedgerHistory, mkRuntimeLedgerHistory )
import Spectrum.EventSource.Data.TxEvent
  ( TxEvent(AppliedTx, UnappliedTx, PendingTx) )
import Spectrum.EventSource.Data.TxContext
  ( TxCtx(LedgerCtx, MempoolCtx) )
import Spectrum.LedgerSync.Data.LedgerUpdate
  ( LedgerUpdate(RollForward, RollBackward) )
import Spectrum.EventSource.Persistence.Data.BlockLinks
  ( BlockLinks(BlockLinks, txIds, prevPoint) )
import Spectrum.LedgerSync.Data.MempoolUpdate
  ( MempoolUpdate(..) )
import Spectrum.EventSource.Persistence.Config
  ( LedgerStoreConfig )
import Spectrum.Prelude.HigherKind
  ( LiftK (liftK), type (~>) )
import qualified Streamly.Internal.Data.Stream.IsStream as S

newtype EventSource s m ctx = EventSource
  { upstream     :: s m (TxEvent ctx)
  }

mkLedgerEventSource
  :: forall f m s env.
    ( Monad f
    , MonadIO f
    , MonadThrow f
    , LiftK m f
    , IsStream s
    , Monad (s m)
    , MonadAsync m
    , MonadReader env f
    , HasType (MakeLogging f m) env
    , HasType EventSourceConfig env
    , HasType LedgerStoreConfig env
    )
  => LedgerSync m
  -> m ~> f
  -> f (EventSource s m 'LedgerCtx)
mkLedgerEventSource lsync fToM = do
  MakeLogging{..}            <- askContext :: f (MakeLogging f m)
  EventSourceConfig{startAt} <- askContext
  -- lhcong                     <- askContext

  logging     <- forComponent "LedgerEventSource"
  persistence <- fToM mkRuntimeLedgerHistory

  liftK $ seekToBeginning logging persistence lsync startAt
  pure $ EventSource
    { upstream = upstream' logging persistence lsync
    }

mkMempoolTxEventSource
  :: forall f m s env.
    ( Monad f
    , IsStream s
    , Monad (s m)
    , MonadAsync m
    , MonadReader env f
    , HasType (MakeLogging f m) env
    )
  => LedgerSync m
  -> f (EventSource s m 'MempoolCtx)
mkMempoolTxEventSource lsync =   do
  MakeLogging{..} <- askContext
  logging         <- forComponent "MempoolEventSource"

  pure $ EventSource
    { upstream = upstreamMempoolTxs' logging lsync
    }

upstream'
  :: forall s m. (IsStream s, Monad (s m), MonadAsync m)
  => Logging m
  -> LedgerHistory m
  -> LedgerSync m
  -> s m (TxEvent 'LedgerCtx)
upstream' logging@Logging{..} persistence LedgerSync{..}
  = S.repeatM pull >>= processUpdate logging persistence
  & S.trace (debugM . show)

upstreamMempoolTxs'
  :: forall s m. (IsStream s, Monad (s m), MonadAsync m)
  => Logging m
  -> LedgerSync m
  -> s m (TxEvent 'MempoolCtx)
upstreamMempoolTxs' logging@Logging{..} LedgerSync{..}
  = S.repeatM pullTx >>= processMempoolUpdate logging
  & S.trace (debugM . show)

processUpdate
  :: forall s m.
    ( IsStream s
    , Monad (s m)
    , MonadIO m
    , MonadThrow m
    )
  => Logging m
  -> LedgerHistory m
  -> LedgerUpdate Block
  -> s m (TxEvent 'LedgerCtx)
processUpdate
  _
  LedgerHistory{..}
  (RollForward block@(BlockBabbage (ShelleyBlock (Ledger.Block (Praos.Header hBody _) txs) hHash)) _) =
    let
      txs'  = txSeqTxns txs
      slotNo = Praos.hbSlotNo hBody
      point = ConcretePoint slotNo (ConcreteHash ch)
        where ch = OneEraHash . toShort . CC.hashToBytes . unShelleyHash $ hHash
      txsParsed = txs' <&> fromBabbageLedgerTx hHash slotNo
      txsIds = Foldable.toList (txsParsed <&> (\(MinimalLedgerTx MinimalConfirmedTx{..}) -> txId)) :: [TxId]
      parsedTxs = txsParsed <&> AppliedTx
    in S.before (setTip point >> putBlock point (BlockLinks point txsIds)) $ S.fromFoldable parsedTxs
processUpdate logging lh (RollBackward point) = streamUnappliedTxs logging lh point
processUpdate Logging{..} _ upd = S.before (errorM $ "Cannot process update " <> show upd) (S.fromList [])

processMempoolUpdate
  :: forall s m.
    ( IsStream s
    , MonadIO m
    )
  => Logging m
  -> MempoolUpdate Block
  -> s m (TxEvent 'MempoolCtx)
processMempoolUpdate _ (NewTx (GenTxBabbage (ShelleyTx _ x)) slot) = S.fromList [PendingTx $ fromMempoolBabbageLedgerTx x slot]
processMempoolUpdate Logging{..} _ = S.before (errorM @String "Cannot process mempool update") $ S.fromList []

streamUnappliedTxs
  :: forall s m.
    ( IsStream s
    , Monad (s m)
    , MonadIO m
    )
  => Logging m
  -> LedgerHistory m
  -> Point Block
  -> s m (TxEvent 'LedgerCtx)
streamUnappliedTxs Logging{..} LedgerHistory{..} point = join $ S.fromEffect $ do
  knownPoint <- pointExists $ fromPoint point
  let
    rollbackOne :: ConcretePoint -> s m (TxEvent 'LedgerCtx)
    rollbackOne pt = do
      block <- S.fromEffect $ getBlock pt
      case block of
        Just BlockLinks{..} -> do
          S.fromEffect $ dropBlock pt >> setTip prevPoint
          let emitTxs = S.fromFoldable (Prelude.reverse txIds <&> UnappliedTx) -- unapply txs in reverse order
          if toPoint prevPoint == point
            then emitTxs
            else S.append emitTxs (rollbackOne prevPoint)
        Nothing -> S.fromList []
  if knownPoint
    then do
      tipM <- getTip
      case tipM of
        Just tip -> infoM ("Rolling back to point " <> show point) $> rollbackOne tip
        Nothing  -> errorM ("An attempt to roll back to an unknown point. (Empty tipM) " <> show point) $> S.fromList []
    else errorM ("An attempt to roll back to an unknown point " <> show point) $> S.fromList []
  -- tipM <- getTip
  -- case tipM of
  --   Just tip ->
  --   Nothing -> pure mempty

seekToBeginning
  :: Monad m
  => Logging m
  -> LedgerHistory m
  -> LedgerSync m
  -> ConcretePoint
  -> m ()
seekToBeginning Logging{..} LedgerHistory{..} LedgerSync{..} pointLowConf = do
  lastCheckpoint <- getTip
  let
    confSlot = slot pointLowConf
    pointLow = fromMaybe pointLowConf
      $ lastCheckpoint <&> (\p -> if confSlot > slot p then pointLowConf else p)
  infoM $ "Seeking to point " <> show pointLow
  seekTo $ toPoint pointLow
