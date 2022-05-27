module NetworkAPI.Node.NodeClient
  ( NodeError(..)
  , NodeClient(..)
  , mkNodeClient
  ) where

import           RIO
import qualified Data.Text            as Text
import           System.Logging.Hlog

import NetworkAPI.Types   (SocketPath(..), SystemEnv(..))

import           Cardano.Api
import qualified Ouroboros.Network.Protocol.LocalTxSubmission.Client as Net.Tx

data NodeError
  = NodeConnectionFailed
  | EraMismatch
  | WrongMode
  | TxSubmissionFailed Text
  deriving (Show, Exception)

data NodeClient f era = NodeClient
  { getSystemEnv :: f SystemEnv
  , submitTx     :: Tx era -> f ()
  }

mkNodeClient
  :: (Monad i, MonadIO f, MonadThrow f)
  => MakeLogging i f
  -> CardanoEra era
  -> ConsensusModeParams CardanoMode
  -> NetworkId
  -> SocketPath
  -> i (NodeClient f era)
mkNodeClient MakeLogging{..} cera cModeParams networkId (SocketPath sockPath) = do
  logging <- forComponent "nodeClient"
  let conn = LocalNodeConnectInfo cModeParams networkId sockPath
  pure $ NodeClient
    { getSystemEnv = getSystemEnv' cera conn
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
