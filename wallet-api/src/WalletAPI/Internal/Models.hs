module WalletAPI.Internal.Models where

import RIO
import Data.Aeson

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
