module Models.Utils
  ( genByteString
  , unsafeFromEither
  ) where

import qualified Data.ByteString         as BS
import qualified Data.ByteString.Base16  as Hex
import qualified Data.Text.Encoding      as E
import qualified Data.Text as T

import qualified System.Random as Random

constStdGen :: Random.StdGen
constStdGen = Random.mkStdGen 100

genRandomString :: String
genRandomString = (take 10 . Random.randomRs ('a','z')) constStdGen

genRandomText :: T.Text
genRandomText = T.pack genRandomString

unsafeFromEither :: (Show b) => Either b a -> a
unsafeFromEither (Left err)    = Prelude.error ("Err:" ++ show err)
unsafeFromEither (Right value) = value

genByteString :: BS.ByteString
genByteString = E.encodeUtf8 genRandomText