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
import WalletAPI.Internal.Crypto

data TrustStoreFile = TrustStoreFile
  { trustStoreSecret :: SecretEnvelope
  , trustStoreVK     :: EncodedVK
  } deriving (Generic, ToJSON, FromJSON)

data SecretEnvelope = SecretEnvelope
  { secretCiphertext :: Ciphertext
  , secretSalt       :: Salt
  , secretIv         :: EncodedIV
  } deriving (Generic, ToJSON, FromJSON)
