module WalletAPI.Internal.Models where

import           Data.ByteString
import qualified Data.ByteString.Base16 as Hex
import           Data.Aeson
import           Data.Text (Text)
import qualified Data.Text.Encoding     as T
import           GHC.Generics           (Generic)

data SecretEnvelope = SecretEnvelope
  { secretEncrypted :: Text
  , secretSalt      :: Text
  , secretIv        :: Text
  } deriving (Generic, ToJSON, FromJSON)

packEnvelope :: ByteString -> ByteString -> ByteString -> SecretEnvelope
packEnvelope secret salt iv = SecretEnvelope (encode' secret) (encode' salt) (encode' iv)
  where encode' = T.decodeUtf8 . Hex.encode

unpackEnvelope :: SecretEnvelope -> Maybe (ByteString, ByteString, ByteString)
unpackEnvelope SecretEnvelope{..} = either (\_ -> Nothing) Just t3
  where
    decode' = Hex.decode . T.encodeUtf8
    t3 = do
      seHex <- decode' secretEncrypted
      ssHex <- decode' secretSalt
      ivHex <- decode' secretIv
      pure (seHex, ssHex, ivHex)
