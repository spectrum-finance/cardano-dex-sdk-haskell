module Spectrum.LedgerSync
  ( LedgerSync(..)
  , mkLedgerSync
  ) where

import RIO ( (<&>), void )

import Spectrum.Prelude.Context
  ( MonadReader, HasType, askContext )
import Spectrum.Prelude.UnliftIO
  ( UnliftIO )

import GHC.Num ( naturalToInt )

import Control.Monad.IO.Class
  ( MonadIO )
import Control.Monad.Class.MonadSTM
  ( MonadSTM (..), TQueue )
import Control.Monad.Class.MonadThrow
  ( MonadThrow (throwIO), MonadMask )
import Control.Monad.Class.MonadST
  ( MonadST )
import Control.Monad.Class.MonadAsync
  ( MonadAsync )
import Control.Monad.Class.MonadFork
  ( MonadFork (forkIO) )

import Control.Tracer
  ( Tracer (..), natTracer)

import System.Logging.Hlog
  ( Logging(Logging, errorM, warnM, infoM, debugM), MakeLogging(..) )

import Spectrum.LedgerSync.Data.LedgerUpdate
  ( LedgerUpdate )
import qualified Spectrum.LedgerSync.Data.LedgerUpdate  as Update
import qualified Spectrum.LedgerSync.Data.MempoolUpdate as MempoolUpdate
import Spectrum.LedgerSync.Protocol.Data.ChainSync
  ( RequestNextResponse(RollBackward, RollForward, block, point),
    RequestNext(RequestNext),
    ChainSyncResponse(RequestNextRes, FindIntersectRes),
    ChainSyncRequest(RequestNextReq, FindIntersectReq),
    FindIntersect(FindIntersect),
    FindIntersectResponse (IntersectionFound) )

import Ouroboros.Consensus.Block
  ( StandardHash )
import Cardano.Network.Protocol.NodeToClient.Trace
  ( TraceClient )
import Ouroboros.Network.Block
  ( Point )
import Ouroboros.Network.NodeToClient.Version
  ( NodeToClientVersionData (NodeToClientVersionData) )
import Ouroboros.Consensus.Cardano.Block 
  ( GenTx )

import Spectrum.LedgerSync.Config
  ( NetworkParameters(NetworkParameters, slotsPerEpoch, networkMagic),
    NodeSocketConfig(..) )
import Spectrum.LedgerSync.Exception
  ( ChainSyncInitFailed(ChainSyncInitFailed) )
import Spectrum.LedgerSync.Protocol.ChainSync
  ( mkChainSyncClient )
import Spectrum.LedgerSync.Protocol.MempoolSync
import Spectrum.LedgerSync.Protocol.Data.MempoolSync
import Spectrum.LedgerSync.Protocol.Client
  ( mkClient, connectClient, Block, Clients(..) )
import Spectrum.Prelude.HigherKind
  ( FunctorK(..) )
import Spectrum.LedgerSync.Data.MempoolUpdate (MempoolUpdate)
import Debug.Trace ()

data LedgerSync m = LedgerSync
  { pull    :: m (LedgerUpdate Block)
  , tryPull :: m (Maybe (LedgerUpdate Block))
  , pullTx  :: m (MempoolUpdate Block)
  , seekTo  :: Point Block -> m ()
  }

instance FunctorK LedgerSync where
  fmapK trans LedgerSync{..} =
    LedgerSync
      { pull    = trans pull
      , tryPull = trans tryPull
      , pullTx  = trans pullTx
      , seekTo  = trans . seekTo
      }

mkLedgerSync
  :: forall m env.
    ( MonadAsync m
    , MonadFork m
    , MonadMask m
    , MonadST m
    , MonadIO m
    , MonadReader env m
    , HasType NodeSocketConfig env
    , HasType NetworkParameters env
    , HasType (MakeLogging m m) env
    )
  => UnliftIO m
  -> Tracer m TraceClient
  -> m (LedgerSync m)
mkLedgerSync unliftIO tr = do
  MakeLogging{..}                               <- askContext
  NodeSocketConfig{nodeSocketPath, maxInFlight} <- askContext
  NetworkParameters{slotsPerEpoch,networkMagic} <- askContext

  l@Logging{..} <- forComponent "LedgerSync"
  (outQ, inQ) <- atomically $ (,) <$> newTQueue <*> newTQueue

  (outQTxMonitor, inQTxMonitor) <- atomically $ (,) <$> newTQueue <*> newTQueue
  let
    chainSyncClient = mkChainSyncClient (naturalToInt maxInFlight) outQ inQ
    txMonitorClient = mkTxMonitorClient outQTxMonitor inQTxMonitor
    clients         = Clients chainSyncClient txMonitorClient

    client          = mkClient unliftIO slotsPerEpoch clients
    versions        = NodeToClientVersionData networkMagic

  infoM @String "Connecting Node Client"
  void $ forkIO $ connectClient (natTracer unliftIO tr) client versions nodeSocketPath
  infoM @String "LedgerSync initialized successfully"
  pure LedgerSync
    { pull    = pull' outQ inQ
    , tryPull = tryPull' outQ inQ
    , pullTx  = pullTx' outQTxMonitor inQTxMonitor
    , seekTo  = seekTo' l outQ inQ
    }

-- | Set chain sync state to the desired block
seekTo'
  :: (MonadSTM m, MonadThrow m, StandardHash block)
  => Logging m
  -> TQueue m (ChainSyncRequest block)
  -> TQueue m (ChainSyncResponse block)
  -> Point block
  -> m ()
seekTo' Logging{..} outQ inQ point = do
  atomically $ writeTQueue outQ $ FindIntersectReq $ FindIntersect [point]
  res <- atomically $ readTQueue inQ
  case res of
    FindIntersectRes (IntersectionFound _ _) -> 
      infoM @String"IntersectionFound!" >> pure ()
    _ -> 
      infoM @String"An attempt to seed to an unknown point" >> (throwIO $ ChainSyncInitFailed $ "An attempt to seed to an unknown point " <> show point)

pull'
  :: MonadSTM m
  => TQueue m (ChainSyncRequest block)
  -> TQueue m (ChainSyncResponse block)
  -> m (LedgerUpdate block)
pull' outQ inQ = do
  atomically $ writeTQueue outQ $ RequestNextReq RequestNext
  atomically $ readTQueue inQ <&> extractUpdate

pullTx'
  :: MonadSTM m
  => TQueue m (MempoolRequest block)
  -> TQueue m (MempoolResponse block)
  -> m (MempoolUpdate block)
pullTx' outQ inQ = do
  atomically $ writeTQueue outQ RequestNextTx
  atomically $ readTQueue inQ <&> extractMempoolUpdate

tryPull'
  :: MonadSTM m
  => TQueue m (ChainSyncRequest block)
  -> TQueue m (ChainSyncResponse block)
  -> m (Maybe (LedgerUpdate block))
tryPull' outQ inQ = do
  atomically $ writeTQueue outQ $ RequestNextReq RequestNext
  atomically $ tryReadTQueue inQ <&> (<&> extractUpdate)

extractUpdate :: ChainSyncResponse block -> LedgerUpdate block
extractUpdate (RequestNextRes RollForward{block})  = Update.RollForward block
extractUpdate (RequestNextRes RollBackward{point}) = Update.RollBackward point
extractUpdate _                                    = undefined

extractMempoolUpdate :: MempoolResponse block -> MempoolUpdate block
extractMempoolUpdate (NewTx tx slot) = MempoolUpdate.NewTx tx slot
extractMempoolUpdate _               = undefined
