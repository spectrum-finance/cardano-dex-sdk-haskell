module WalletAPI.Internal.TrustStore where

import qualified Data.ByteString        as BS
import           Control.Monad.IO.Class
import           Data.Text
import           Data.Functor

import           Crypto.Cipher.AES   (AES256)
import qualified Cardano.Crypto.Seed as Crypto
import qualified Cardano.Api         as Crypto

import WalletAPI.Internal.Crypto
import WalletAPI.Internal.Models

data SecretFile = SecretFile { unSigningKeyFile :: FilePath }

newtype KeyPass = KeyPass { unKeyPass :: Text }

data TrustStore f = TrustStore
  { readKey :: KeyPass -> f (Crypto.SigningKey Crypto.PaymentKey)
  }

mkTrustStore :: MonadIO f => SecretFile -> f (TrustStore f)
mkTrustStore secretPath = do
  maybeTrustStore <- readTrustStore secretPath
  case maybeTrustStore of
    Just ts -> TrustStore (\pass -> )

initTrustStore :: MonadIO f => SecretFile -> f SecretEnvelope

readTrustStore :: MonadIO f => SecretFile -> f (Maybe SecretEnvelope)

decryptKey :: ByteString -> KeyPass -> Maybe (Crypto.SigningKey Crypto.PaymentKey)

encryptKey :: Crypto.SigningKey Crypto.PaymentKey -> KeyPass -> ByteString

-- initTrustStore' :: MonadIO f => FilePath -> f ()
-- initTrustStore' path = liftIO $ Crypto.generateSigningKey Crypto.AsPaymentKey >>= writeSigningKey path
--
-- writeSigningKey :: MonadIO f => FilePath -> Crypto.SigningKey Crypto.PaymentKey -> f ()
-- writeSigningKey dest key = liftIO $ BS.writeFile dest (Crypto.serialiseToRawBytes key)
--
-- readSigningKey :: MonadIO f => FilePath -> f (Maybe (Crypto.SigningKey Crypto.PaymentKey))
-- readSigningKey path = liftIO $ BS.readFile path <&> Crypto.deserialiseFromRawBytes asSk
--   where asSk = Crypto.AsSigningKey Crypto.AsPaymentKey
