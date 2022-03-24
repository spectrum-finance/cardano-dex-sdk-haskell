{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}

module WalletAPI.Internal.Crypto where

import           Crypto.Cipher.Types (BlockCipher(..), Cipher(..), IV, makeIV)
import           Crypto.Error        (CryptoFailable(..), CryptoError(..))

import qualified Crypto.Random.Types as CRT

import           Data.ByteArray          (ByteArray)
import           Data.ByteString         (ByteString)
import qualified Data.ByteString.Base16  as Hex
import           Data.Aeson
import qualified Data.Text.Encoding      as T

data Key c a where
  Key :: (BlockCipher c, ByteArray a) => a -> Key c a

newtype Salt = Salt { unSalt :: ByteString }

instance ToJSON Salt where
  toJSON = toJSON . T.decodeUtf8 . Hex.encode . unSalt

instance FromJSON Salt where
  parseJSON (String s) = either fail (pure . Salt) (Hex.decode . T.encodeUtf8 $ s)
  parseJSON _          = fail "Expected a string"

newtype Ciphertext = Ciphertext { unCyphertext :: ByteString }

instance ToJSON Ciphertext where
  toJSON = toJSON . T.decodeUtf8 . Hex.encode . unCyphertext

instance FromJSON Ciphertext where
  parseJSON (String s) = either fail (pure . Ciphertext) (Hex.decode . T.encodeUtf8 $ s)
  parseJSON _          = fail "Expected a string"

newtype EncodedVK = EncodedVK { unEncodedVK :: ByteString }

instance ToJSON EncodedVK where
  toJSON = toJSON . T.decodeUtf8 . Hex.encode . unEncodedVK

instance FromJSON EncodedVK where
  parseJSON (String s) = either fail (pure . EncodedVK) (Hex.decode . T.encodeUtf8 $ s)
  parseJSON _          = fail "Expected a string"

newtype EncodedIV = EncodedIV { unEncodedIV :: ByteString }

instance ToJSON EncodedIV where
  toJSON = toJSON . T.decodeUtf8 . Hex.encode . unEncodedIV

instance FromJSON EncodedIV where
  parseJSON (String s) = either fail (pure . EncodedIV) (Hex.decode . T.encodeUtf8 $ s)
  parseJSON _          = fail "Expected a string"

-- | Generate a random initialization vector for a given block cipher
genRandomIV :: forall f c. (CRT.MonadRandom f, BlockCipher c) => f (Maybe (IV c))
genRandomIV = do
  bytes :: ByteString <- CRT.getRandomBytes $ blockSize (undefined :: c)
  pure $ makeIV bytes

-- | Generate random salt of a given size
genRandomSalt :: forall f. (CRT.MonadRandom f) => Int -> f Salt
genRandomSalt = fmap Salt . CRT.getRandomBytes

-- | Initialize a block cipher
initCipher :: (BlockCipher c, ByteArray a) => Key c a -> Either CryptoError c
initCipher (Key k) = case cipherInit k of
  CryptoFailed e -> Left e
  CryptoPassed a -> Right a

encrypt :: (BlockCipher c, ByteArray a) => Key c a -> IV c -> a -> Either CryptoError a
encrypt secretKey initIV msg =
  case initCipher secretKey of
    Left e  -> Left e
    Right c -> Right $ ctrCombine c initIV msg

decrypt :: (BlockCipher c, ByteArray a) => Key c a -> IV c -> a -> Either CryptoError a
decrypt = encrypt
