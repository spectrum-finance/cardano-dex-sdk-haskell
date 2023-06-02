{-# LANGUAGE RecordWildCards #-}

module Spectrum.LedgerSync.Protocol.MempoolSync where

import Control.Monad.Class.MonadSTM
  ( MonadSTM (..), TQueue )
import Spectrum.LedgerSync.Protocol.Data.MempoolSync
  ( MempoolRequest(..), MempoolResponse(..) )
import Ouroboros.Network.Protocol.LocalTxMonitor.Client
import Ouroboros.Consensus.Ledger.SupportsMempool
  ( GenTx, GenTxId )
import Cardano.Api.Shelley
  ( SlotNo )

mkTxMonitorClient
  :: forall m block. MonadSTM m
  => TQueue m (MempoolRequest block)
  -> TQueue m (MempoolResponse block)
  -> LocalTxMonitorClient (GenTxId block) (GenTx block) SlotNo m ()
mkTxMonitorClient incomingQ outgoingQ =
    LocalTxMonitorClient clientStIdle
  where
    pull :: m (MempoolRequest block)
    pull = do
      atomically (readTQueue incomingQ)

    clientStIdle :: m (ClientStIdle (GenTxId block) (GenTx block) SlotNo m ())
    clientStIdle = pull >>= (\case
        RequestNextTx -> do
          pure $ SendMsgAcquire $ \slot -> clientStAcquired (Just RequestNextTx) slot
        _ -> clientStIdle
      )

    clientStAcquired :: Maybe (MempoolRequest block) -> SlotNo -> m (ClientStAcquired (GenTxId block) (GenTx block) SlotNo m ())
    clientStAcquired prevRequestM slotNo = case prevRequestM of
        Just prevRequest -> executeRequest prevRequest
        Nothing -> pull >>= executeRequest
      where
        executeRequest = \case
          RequestNextTx -> do
            pure $ SendMsgNextTx $ \case
              Nothing -> do
                pure $ SendMsgAwaitAcquire $ const $ clientStAcquired (Just RequestNextTx) slotNo
              Just tx -> do
                atomically $ writeTQueue outgoingQ $ NewTx tx slotNo
                clientStAcquired Nothing slotNo
          _ -> clientStAcquired Nothing slotNo
