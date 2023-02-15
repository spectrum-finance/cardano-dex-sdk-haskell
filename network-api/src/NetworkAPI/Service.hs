module NetworkAPI.Service
  ( NodeError(..)
  , CardanoNetwork(..)
  , mkCardanoNetwork
  ) where

import RIO

import qualified Data.Text as Text

import System.Logging.Hlog (Logging(..), MakeLogging(..))

import           Cardano.Api hiding (SocketPath)
import qualified Ouroboros.Network.Protocol.LocalTxSubmission.Client as Net.Tx

import NetworkAPI.Types (SocketPath(..), SystemEnv(..))

data NodeError
  = NodeConnectionFailed
  | EraMismatch
  | WrongMode
  | TxSubmissionFailed Text
  deriving (Show, Exception)

data CardanoNetwork f era = CardanoNetwork
  { getSystemEnv :: f SystemEnv
  , submitTx     :: Tx era -> f ()
  }

mkCardanoNetwork
  :: (MonadIO i, MonadThrow f, MonadUnliftIO f)
  => MakeLogging i f
  -> CardanoEra era
  -> ConsensusModeParams CardanoMode
  -> NetworkId
  -> SocketPath
  -> i (CardanoNetwork f era)
mkCardanoNetwork MakeLogging{..} cera cModeParams networkId (SocketPath sockPath) = do
  logging <- forComponent "CardanoNetwork"
  let conn = LocalNodeConnectInfo cModeParams networkId sockPath
  emptyMVar <- newEmptyMVar
  pure $ CardanoNetwork
    { getSystemEnv = withAsyncCache emptyMVar $ getSystemEnv' cera conn
    , submitTx     = submitTx' logging cera conn
    }

getSystemEnv'
  :: (MonadIO f, MonadThrow f)
  => CardanoEra era
  -> LocalNodeConnectInfo CardanoMode
  -> f SystemEnv
getSystemEnv' era conn =
  case (cardanoEraStyle era, toEraInMode era CardanoMode) of
    (ShelleyBasedEra sbe, Just eInMode) ->
      either (const $ throwM EraMismatch) pure =<<
        either (const $ throwM NodeConnectionFailed) pure =<<
          liftIO (executeLocalStateQueryExpr conn Nothing $ \_ntcVersion -> do
            pparams     <- queryExpr $ QueryInEra eInMode $ QueryInShelleyBasedEra sbe QueryProtocolParameters
            eraHistory  <- queryExpr $ QueryEraHistory CardanoModeIsMultiEra
            systemStart <- queryExpr QuerySystemStart
            stakePools  <- queryExpr . QueryInEra eInMode . QueryInShelleyBasedEra sbe $ QueryStakePools

            pure $ do
              pparams' <- pparams
              pools'   <- stakePools
              pure $ SystemEnv pparams' systemStart pools' eraHistory
          )
    _ -> throwM WrongMode

submitTx'
  :: (MonadIO f, MonadThrow f)
  => Logging f
  -> CardanoEra era
  -> LocalNodeConnectInfo CardanoMode
  -> Tx era
  -> f ()
submitTx' Logging{..} era conn tx =
  case toEraInMode era CardanoMode of
    Just eraInMode -> do
      let txInMode = TxInMode tx eraInMode
      res <- liftIO $ submitTxToNodeLocal conn txInMode
      case res of
        Net.Tx.SubmitSuccess     -> infoM @String "Transaction successfully submitted."
        Net.Tx.SubmitFail reason ->
          case reason of
            TxValidationErrorInMode err _ -> throwM $ TxSubmissionFailed $ Text.pack $ show err
            TxValidationEraMismatch _     -> throwM EraMismatch
    _ -> throwM WrongMode

withAsyncCache
   :: (MonadIO f, MonadUnliftIO f)
   => MVar a
   -> f a
   -> f a
withAsyncCache mVar fa = do
  mVarReadResult <- liftIO $ tryTakeMVar mVar
  case mVarReadResult of
    Nothing -> do
      a  <- fa
      _  <- liftIO $ tryPutMVar mVar a
      return a
    Just env -> async (updateAsyncCache' mVar fa) >> pure env

updateAsyncCache'
  :: (MonadIO f, MonadUnliftIO f)
  => MVar a
  -> f a
  -> f ()
updateAsyncCache' mVar fa = fa >>= (void . liftIO . tryPutMVar mVar)
