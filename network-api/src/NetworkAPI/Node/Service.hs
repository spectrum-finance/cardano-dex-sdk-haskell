module NetworkAPI.Node.Service
  ( NodeError(..)
  , Network(..)
  , mkNetwork
  ) where

import           RIO
import qualified Data.Text as Text

import NetworkAPI.Service (Network(..))
import NetworkAPI.Types   (SocketPath(..), SystemEnv(..))

import           Cardano.Api
import qualified Ouroboros.Network.Protocol.LocalTxSubmission.Client as Net.Tx

data NodeError
  = NodeConnectionFailed
  | EraMismatch
  | WrongMode
  | TxSubmissionFailed Text
  deriving (Show, Exception)

mkNetwork
  :: (MonadIO f, MonadThrow f)
  => CardanoEra era
  -> ConsensusModeParams CardanoMode
  -> NetworkId
  -> SocketPath
  -> Network f era
mkNetwork cera cModeParams networkId (SocketPath sockPath) =
  let conn = LocalNodeConnectInfo cModeParams networkId sockPath
  in Network
    { getSystemEnv = getSystemEnv' cera conn
    , submitTx     = submitTx' cera conn
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
  => CardanoEra era
  -> LocalNodeConnectInfo CardanoMode
  -> Tx era
  -> f ()
submitTx' era conn tx =
  case toEraInMode era CardanoMode of
    Just eraInMode -> do
      let txInMode = TxInMode tx eraInMode
      res <- liftIO $ submitTxToNodeLocal conn txInMode
      case res of
        Net.Tx.SubmitSuccess     -> liftIO $ putStrLn "Transaction successfully submitted."
        Net.Tx.SubmitFail reason ->
          case reason of
            TxValidationErrorInMode err _ -> throwM $ TxSubmissionFailed $ Text.pack $ show err
            TxValidationEraMismatch _     -> throwM EraMismatch
    _ -> throwM WrongMode
