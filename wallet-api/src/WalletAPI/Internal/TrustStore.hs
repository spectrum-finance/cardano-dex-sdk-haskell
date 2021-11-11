module WalletAPI.Internal.TrustStore where

import           RIO
import qualified Data.ByteString        as BS
import qualified Data.ByteString.Lazy   as BL
import qualified Data.ByteArray         as BA
import qualified Data.Text.Encoding     as T
import           Data.Aeson

import qualified Crypto.Hash         as H
import           Crypto.Cipher.AES   (AES256)
import           Crypto.Random.Types
import qualified Cardano.Api         as Crypto

import WalletAPI.Internal.Crypto
import WalletAPI.Internal.Models

data SecretFile = SecretFile { unSigningKeyFile :: FilePath }

newtype KeyPass = KeyPass { unKeyPass :: Text }

data KeyLookupError =
    DecryptionFailed
  | NotInitialized
  deriving (Show, Exception)

data InitializationError =
    InitializationError
  | AlreadyInitialized
  deriving (Show, Exception)

data TrustStore f = TrustStore
  { init    :: KeyPass -> f ()
  , readKey :: KeyPass -> f (Crypto.SigningKey Crypto.PaymentKey)
  }

mkTrustStore
  :: (MonadIO f, MonadThrow f, MonadRandom f)
  => SecretFile
  -> TrustStore f
mkTrustStore file = TrustStore
  { init    = init' file
  , readKey = readKey' file
  }

init'
  :: (MonadIO f, MonadThrow f, MonadRandom f)
  => SecretFile
  -> KeyPass
  -> f ()
init' file pass = do
  sk       <- liftIO $ Crypto.generateSigningKey Crypto.AsPaymentKey
  envelope <- encryptKey sk pass
  writeEnvelope file envelope

readKey'
  :: (MonadIO f, MonadThrow f)
  => SecretFile
  -> KeyPass
  -> f (Crypto.SigningKey Crypto.PaymentKey)
readKey' file pass = do
  envelope <- readEnvelope file >>= maybe (throwM NotInitialized) pure
  maybe (throwM DecryptionFailed) pure $ decryptKey envelope pass

decryptKey :: SecretEnvelope -> KeyPass -> Maybe (Crypto.SigningKey Crypto.PaymentKey)
decryptKey envelope pass = do
  (text, salt, iv) <- unpackEnvelope (undefined :: AES256) envelope
  let encryptionKey = mkEncryptionKey pass salt
  rawKey <- either (\_ -> Nothing) Just $ decrypt encryptionKey iv text

  Crypto.deserialiseFromRawBytes asSk rawKey
    where asSk = Crypto.AsSigningKey Crypto.AsPaymentKey

encryptKey
  :: (MonadIO f, MonadThrow f, MonadRandom f)
  => Crypto.SigningKey Crypto.PaymentKey
  -> KeyPass
  -> f SecretEnvelope
encryptKey sk pass = do
  salt <- genRandomSalt 16
  iv   <- genRandomIV (undefined :: AES256) >>= maybe (throwM InitializationError) pure
  let
    encryptionKey = mkEncryptionKey pass salt
    rawSk         = Crypto.serialiseToRawBytes sk
  ciphertext <- either (\_ -> throwM InitializationError) pure $ encrypt encryptionKey iv rawSk

  pure $ packEnvelope ciphertext salt iv

mkEncryptionKey :: KeyPass -> Salt -> Key AES256 ByteString
mkEncryptionKey (KeyPass pass) (Salt salt) =
  Key $ BS.pack $ BA.unpack $ H.hashWith H.SHA256 $ T.encodeUtf8 pass <> salt

writeEnvelope :: MonadIO f => SecretFile -> SecretEnvelope -> f ()
writeEnvelope (SecretFile path) envelope =
  liftIO $ BL.writeFile path (encode envelope)

readEnvelope :: MonadIO f => SecretFile -> f (Maybe SecretEnvelope)
readEnvelope (SecretFile path) =
  liftIO $ BL.readFile path <&> decode
