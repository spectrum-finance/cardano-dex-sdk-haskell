module WalletAPI.Internal.TrustStore where

import qualified Data.ByteString        as BS
import           Control.Monad.IO.Class
import           Data.Text
import           Data.Functor

import qualified Cardano.Crypto.Seed as Crypto
import qualified Cardano.Api         as Crypto

data SigningKeyFile = SigningKeyFile { unSigningKeyFile :: FilePath }

newtype SeedPass = SeedPass { unSeedPass :: Text }

data TrustStore f = TrustStore
  { readKey :: SeedPass -> f Crypto.SigningKey Crypto.PaymentKey
  }

initTrustStore :: MonadIO f => FilePath -> f ()
initTrustStore path = liftIO $ Crypto.generateSigningKey Crypto.AsPaymentKey >>= writeSigningKey path

writeSigningKey :: MonadIO f => FilePath -> Crypto.SigningKey Crypto.PaymentKey -> f ()
writeSigningKey dest key = liftIO $ BS.writeFile dest (Crypto.serialiseToRawBytes key)

readSigningKey :: MonadIO f => FilePath -> f (Maybe (Crypto.SigningKey Crypto.PaymentKey))
readSigningKey path = liftIO $ BS.readFile path <&> Crypto.deserialiseFromRawBytes asSk
  where asSk = Crypto.AsSigningKey Crypto.AsPaymentKey
