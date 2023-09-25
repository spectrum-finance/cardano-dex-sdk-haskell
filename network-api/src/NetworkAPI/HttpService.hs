module NetworkAPI.HttpService where

import RIO

import System.Logging.Hlog (Logging(..), MakeLogging(..))
import Network.HTTP.Simple
import           Cardano.Api hiding (SocketPath)
import qualified Data.ByteString.Lazy as LBS
import qualified Cardano.Api                 as C

import Network.HTTP.Client.Conduit (Request(..))

import Dhall
  ( FromDhall)
import Data.Text (isInfixOf, pack)

data LocalSubmitException = BadInputsUtxo Text | BudgetError Text
    deriving Show

instance Exception LocalSubmitException

data HttpServiceConfig = HttpServiceConfig
  { submitUri :: String
  } deriving (Generic, FromDhall)

data CardanoHttpNetwork f era = CardanoHttpNetwork
  { submitTx     :: Tx era -> f C.TxId
  }

mkHttpCardanoNetwork
  :: (MonadIO i, MonadThrow f, MonadUnliftIO f)
  => MakeLogging i f
  -> HttpServiceConfig
  -> i (CardanoHttpNetwork f C.BabbageEra)
mkHttpCardanoNetwork MakeLogging{..} config = do
  logging <- forComponent "CardanoNetwork"
  pure $ CardanoHttpNetwork
    { submitTx  = submitTx' logging config
    }

submitTx'
  :: (MonadIO f, MonadThrow f)
  => Logging f
  -> HttpServiceConfig
  -> C.Tx C.BabbageEra
  -> f C.TxId
submitTx' l@Logging{..} cfg tx = do
  let TextEnvelope{..} = serialiseToTextEnvelope Nothing tx
  mkPostRequest l cfg teRawCBOR >>= (\res -> infoM ("Submit result: " ++ show res)) >> pure (C.TxId "ea0f9abf50d396652d959ba5b2fde9409929043ff7f099e373e66f2226458a02")

mkPostRequest :: (MonadIO f, MonadThrow f) => Logging f -> HttpServiceConfig -> ByteString -> f String
mkPostRequest Logging{..} HttpServiceConfig{..} tx = do
  request <- parseRequest submitUri

  let req = addRequestHeader "Content-Type" "application/cbor" $ setRequestBodyLBS (LBS.fromStrict tx) $ request
            { method = "POST"
            }

  response <- httpJSON req

  let parsedResponse = getResponseBody response

  if pack "BadInputsUTxO" `isInfixOf` pack parsedResponse
    then throwM (BadInputsUtxo (pack parsedResponse))
    else if "The budget when the machine terminated was" `isInfixOf` pack parsedResponse
      then throwM (BudgetError (pack parsedResponse))
      else pure ()

  pure parsedResponse