{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Data.Text.Encoding as E

import Test.Tasty
import Test.Tasty.HUnit
import PlutusTx.Builtins.Internal
import PlutusTx
import ErgoDex.Contracts.Pool
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base16  as Hex
import qualified Data.Text.Encoding      as T
import qualified Data.ByteString.Short  as SBS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString as BS
import Codec.Serialise (serialise)
import Data.Aeson as Json ( encode )
import qualified Data.Text.Encoding      as E
import qualified Data.ByteString.Base16  as Hex
import qualified Data.Text as T
import Plutus.V1.Ledger.Value (AssetClass(..), assetClassValueOf, flattenValue, CurrencySymbol(..), TokenName(..))
import Plutus.V1.Ledger.Api
import ErgoDex.PValidators
import Cardano.Api (writeFileTextEnvelope, Error(displayError), ScriptDataJsonSchema (ScriptDataJsonDetailedSchema))
import Cardano.Api.Shelley (PlutusScript (..), PlutusScriptV2)
import qualified Plutus.V1.Ledger.Scripts as Plutus
import qualified Cardano.Api as C
import qualified Plutus.Script.Utils.V2.Address  as PV2
import Cardano.Api (scriptDataToJson)
import Cardano.Api.Shelley ( fromPlutusData )
import WalletAPI.TrustStore (importTrustStoreFromCardano, SecretFile (SecretFile), KeyPass (KeyPass))
import ErgoDex.Contracts.Proxy.Vesting
import ErgoDex.Contracts.Proxy.VestingWithPeriod

-- vestingCreation | vestingRedeem

actionType = vestingCreation

--------------------------------- Vesting redeemer

vestingIndexInputIx = 0

--------------------------------- Vesting datum

vestingStartTime = POSIXTime 1696118399000  

---------------------------------

vestingPeriodDurationTime = POSIXTime 0

---------------------------------

totalVestedQty = 591133759047619

---------------------------------

periodVestedQty :: Integer
periodVestedQty = 591133759047619

---------------------------------

pubkeyHash = PubKeyHash $ BuiltinByteString $ mkByteString $ T.pack "277c2d03d8115bf540c9e9dc86bd8fdaaaeaddb72527d90eff227a21"

---------------------------------

vestingTN = TokenName $ BuiltinByteString $ mkByteString $ T.pack "535046"

vestincCS = CurrencySymbol $ BuiltinByteString $ mkByteString $ T.pack "05e82386e53993e264c4856ea99d8802492d4670874e3141c4ee3c02"

--------------------------------

vestingPeriodIndex = 1

---------------------------------

vestingInstance = do
  pscript <- vestingWithPeriodValidator
  let
    shortBS = SBS.toShort . LBS.toStrict $ serialise (unValidatorScript pscript)
    scr :: PlutusScript PlutusScriptV2
    scr = PlutusScriptSerialised shortBS
  writeFileTextEnvelope "/home/bromel/projects/cardano-dex-sdk-haskell/submit-api/vesting.plutus" Nothing scr

vestingCreation = writeDataDatum2 "/home/bromel/projects/cardano-dex-sdk-haskell/submit-api/vestingWithPeriodDatum.json"

vestingRedeem = writeDataDatum2Redeem "/home/bromel/projects/cardano-dex-sdk-haskell/submit-api/vestingWithPeriodRedeemer.json"

main :: IO ()
main = do
  --vestingInstance
  actionType
  pure ()

vestingAssetClass = mkAssetClass vestincCS vestingTN

vestingWithPeriodDatum = VestingWithPeriodConfig vestingStartTime vestingPeriodDurationTime totalVestedQty periodVestedQty [pubkeyHash] vestingAssetClass

vestingWithPeriodRedeemer = VestingWithPeriodRedeemer vestingIndexInputIx vestingPeriodIndex

mkByteString :: T.Text -> BS.ByteString
mkByteString input = unsafeFromEither (Hex.decode . E.encodeUtf8 $ input)

mkAssetClass :: CurrencySymbol -> TokenName -> AssetClass
mkAssetClass cs tn = AssetClass (cs, tn)

unsafeFromEither :: (Show b) => Either b a -> a
unsafeFromEither (Left err)    = Prelude.error ("Err:" ++ show err)
unsafeFromEither (Right value) = value

writeDataDatum2 :: FilePath -> IO ()
writeDataDatum2 file = do
  LBS.writeFile file (Json.encode
    . scriptDataToJson ScriptDataJsonDetailedSchema
    . fromPlutusData
    . toData 
    $ (vestingWithPeriodDatum) )

writeDataDatum2Redeem :: FilePath -> IO ()
writeDataDatum2Redeem file = do
  LBS.writeFile file (Json.encode
    . scriptDataToJson ScriptDataJsonDetailedSchema
    . fromPlutusData
    . toData 
    $ (vestingWithPeriodRedeemer) )