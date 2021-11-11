module WalletAPI.Internal.Models where

import           RIO
import qualified Data.ByteString         as BS
import qualified Data.ByteArray          as BA
import qualified Data.ByteString.Base16  as Hex
import           Data.Aeson
import           Data.Text (Text)
import qualified Data.Text.Encoding      as T
import           Data.Either.Combinators
import           GHC.Generics            (Generic)

import Crypto.Cipher.Types       (IV, BlockCipher, makeIV)
import WalletAPI.Internal.Crypto (Salt(..))

data SecretEnvelope = SecretEnvelope
  { secretCiphertext :: Text
  , secretSalt       :: Text
  , secretIv         :: Text
  } deriving (Generic, ToJSON, FromJSON)

packEnvelope :: forall c. BlockCipher c => BS.ByteString -> Salt -> IV c -> SecretEnvelope
packEnvelope text (Salt salt) iv = SecretEnvelope (encode' text) (encode' salt) (encode' iv')
  where
    encode' = T.decodeUtf8 . Hex.encode
    iv'     = BS.pack $ BA.unpack iv

unpackEnvelope :: BlockCipher c => c -> SecretEnvelope -> Maybe (BS.ByteString, Salt, IV c)
unpackEnvelope _ SecretEnvelope{..} = either (\_ -> Nothing) Just t3
  where
    decode' = Hex.decode . T.encodeUtf8
    t3 = do
      seHex <- decode' secretCiphertext
      ssHex <- decode' secretSalt
      ivHex <- decode' secretIv
      maybeToRight "" (makeIV ivHex <&> (seHex, Salt ssHex,))

