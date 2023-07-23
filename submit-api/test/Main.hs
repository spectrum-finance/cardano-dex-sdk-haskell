{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Main where

import qualified Data.Text.Encoding as E

import Test.Tasty
import Test.Tasty.HUnit
import PlutusTx.Builtins.Internal hiding (fst)
import PlutusTx
import ErgoDex.Contracts.Pool
import CardanoTx.Address
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base16  as Hex
import qualified Data.Text.Encoding      as T
import qualified Data.ByteString.Short  as SBS
import qualified Data.ByteString.Lazy as LBS
import Codec.Serialise (serialise, deserialise)
import Data.Aeson as Json ( encode )
import qualified Data.Text.Encoding      as E
import qualified Data.ByteString.Base16  as Hex
import Plutus.Script.Utils.V2.Scripts
import qualified Plutus.V2.Ledger.Api as PlutusV2
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
import qualified Ledger as PV2
import CardanoTx.Address (readShellyAddress)
import Plutus.Script.Utils.V1.Address (mkValidatorAddress)
import Hedgehog.Internal.Show (Value(Integer))


data TokenInfo = TokenInfo
  { curSymbol :: String
  , tokenName :: String
  } deriving Eq

instance Show TokenInfo where
  show TokenInfo{..} = curSymbol ++ "." ++ tokenName

adaTokenInfo :: TokenInfo
adaTokenInfo = TokenInfo "" ""

isAda :: TokenInfo -> Bool
isAda ti = ti == adaTokenInfo

data PoolInfo = PoolInfo
  { name     :: String
  , tokenX   :: TokenInfo
  , tokenY   :: TokenInfo
  , tokenNft :: TokenInfo
  , tokenLP  :: TokenInfo
  , lqBound  :: Integer
  , authKeys :: [String]
  , threshold :: Integer
  , initialXQty :: Integer
  , initialYQty :: Integer
  , allowStaking :: Bool
  }

lqInitQty = 9223372036854775807

workDir :: String
workDir = "/home/bromel/test-mainnet-pools/"

mintingPolicyNamePostfix :: String
mintingPolicyNamePostfix = "_mintingPolicy"

stakingScriptNamePostfix :: String
stakingScriptNamePostfix = "_stakingScript"

poolDatumPostfix :: String
poolDatumPostfix = "_poolDatum"

uplcExtension :: String
uplcExtension = ".uplc"

plutusExtension :: String
plutusExtension = ".plutus"

jsonExtension :: String
jsonExtension = ".json"

uplcPolicyPath :: PoolInfo -> String
uplcPolicyPath PoolInfo{..} = workDir ++ name ++ mintingPolicyNamePostfix ++ uplcExtension

plutusPolicyPath :: PoolInfo -> String
plutusPolicyPath PoolInfo{..} = workDir ++ name ++ mintingPolicyNamePostfix ++ plutusExtension

uplcStakingScriptPath :: String -> String
uplcStakingScriptPath pkh = workDir ++ pkh ++ stakingScriptNamePostfix ++ uplcExtension

plutusStakingScriptPath ::  String -> String
plutusStakingScriptPath pkh = workDir ++ pkh ++ stakingScriptNamePostfix ++ plutusExtension

poolDatumPath :: PoolInfo -> String
poolDatumPath PoolInfo{..} = workDir ++ name ++ poolDatumPostfix ++ jsonExtension

poolMainnetServerDatumPath :: String -> PoolInfo -> String
poolMainnetServerDatumPath mainnetWorkDir PoolInfo{..} = mainnetWorkDir ++ name ++ poolDatumPostfix ++ jsonExtension

dq = "\""

main :: IO ()
main = do
    -- defaultMain tests
  let
    wallet1PubKeyHash = "9b697975d20d891cc1713a3c4d3f881490880a780019337037ef079c"
    wallet2PubKeyHash = "a6e1973e53af80c473cafb288235864f66240d305d9ce9df992125ea"

    mintingSignatures   = [wallet1PubKeyHash, wallet2PubKeyHash]

    signaturesThreshold = 2

    lqQty = 9223372036854775807

    snekPool = PoolInfo
      { name     = "snekPool"
      , tokenX   = adaTokenInfo
      , tokenY   = TokenInfo "279c909f348e533da5808898f87f9a14bb2c3dfbbacccd631d927a3f" "534e454b"
      , tokenNft = TokenInfo "5ac3d4bdca238105a040a565e5d7e734b7c9e1630aec7650e809e34a" "4144415f20534e454b5f4e4654"
      , tokenLP  = TokenInfo "5ac3d4bdca238105a040a565e5d7e734b7c9e1630aec7650e809e34a" "4144415f20534e454b5f4c51"
      , lqBound  = 1000
      , authKeys = []
      , threshold = signaturesThreshold
      , initialXQty = 4000000
      , initialYQty = 4000
      , allowStaking = True
      }
    sundaePool = PoolInfo
      { name     = "sundaePool"
      , tokenX   = adaTokenInfo
      , tokenY   = TokenInfo "9a9693a9a37912a5097918f97918d15240c92ab729a0b7c4aa144d77" "53554e444145"
      , tokenNft = TokenInfo "5ac3d4bdca238105a040a565e5d7e734b7c9e1630aec7650e809e34a" "4144415f53554e4441455f4e4654"
      , tokenLP  = TokenInfo "5ac3d4bdca238105a040a565e5d7e734b7c9e1630aec7650e809e34a" "4144415F53554E4441455F4C51"
      , lqBound  = 5000000
      , authKeys = []
      , threshold = signaturesThreshold
      , initialXQty = 10000000
      , initialYQty = 1000000
      , allowStaking = True
      }
    snekSundaePool = PoolInfo
      { name     = "snekSundaePool"
      , tokenX   = TokenInfo "279c909f348e533da5808898f87f9a14bb2c3dfbbacccd631d927a3f" "534e454b"
      , tokenY   = TokenInfo "9a9693a9a37912a5097918f97918d15240c92ab729a0b7c4aa144d77" "53554e444145"
      , tokenNft = TokenInfo "5ac3d4bdca238105a040a565e5d7e734b7c9e1630aec7650e809e34a" "534E454B5F53554E4441455F4E4654"
      , tokenLP  = TokenInfo "5ac3d4bdca238105a040a565e5d7e734b7c9e1630aec7650e809e34a" "534E454B5F53554E4441455F4C51"
      , lqBound  = 5000000
      , authKeys = []
      , threshold = signaturesThreshold
      , initialXQty = 900
      , initialYQty = 3000000
      , allowStaking = False
      }
    pools = [snekPool, sundaePool, snekSundaePool]

  -- Step 1. Converting uplc produced by cardano-contracts onchain repo to plutus

  -- convertUplcMintingPolicy `traverse` pools

  -- Step 1.5 (optional)

  -- convertUplcStakingScript wallet2PubKeyHash

  -- Step 2. Datums creation. Will produce output for cardano-cli. Also necessary to copy all datums from test to mainnet machine

  -- createDatumJson `traverse` pools

  -- Step 3. Require manual steps for creation staking certs
  -- Also we cannot retrive original min utxo value for inline datums. So, set it manually
  -- More Also, we cannot determine change. So, set it manually too

  -- let
  --   poolAddressWithStaking = "addr1x9cgs59t2hr5sphrv4gfuzxl323akly5z57qv07hq266evkqwx9ghwy6quk2fhu5g0ek8rth7z4zxr5ev975ph34q5fsq2amyd"
  --   poolAddress            = "addr1w9cgs59t2hr5sphrv4gfuzxl323akly5z57qv07hq266evsg37dfw"
  --   -- on mainnet machine
  --   origDatumWorkDir       = "/root/plutus-scripts-for-mainnet/test-pools/datums/"

  --   bootstrapAddressString   = ""
  --   bootstrapAddressVKeyPath = ""

  --   minUtxoValueForPool    = 3223960

  --   res = (\pi -> poolCLICreationOutput pi poolAddressWithStaking poolAddress minUtxoValueForPool origDatumWorkDir) `fmap` pools

  --   folded = foldr (\acc nextPool -> acc ++ "\n" ++ nextPool) "" res

  -- putStr folded

  -- end

  testJson

  pool <- poolValidator
  swap <- swapValidator
  redeem <- redeemValidator
  deposit <- depositValidator

  print $ mkValidatorAddress pool
  print $ mkValidatorAddress swap
  print $ mkValidatorAddress redeem
  print $ mkValidatorAddress deposit

  print $ readShellyAddress "addr1x9cgs59t2hr5sphrv4gfuzxl323akly5z57qv07hq266evkqwx9ghwy6quk2fhu5g0ek8rth7z4zxr5ev975ph34q5fsq2amyd"

  pure ()

-- poolDatum = PoolConfig spectrumTokenNFTClass spectrumTokenAClass spectrumTokenBClass spectrumTokenLPClass 995

-- poolDatumData = toData poolDatum

-- datumStr = (T.decodeUtf8 . Hex.encode $ (LBS.toStrict $ serialise $ poolDatumData))

--- Pool creation stuff ---

-- return cardano-cli string for pool and lp charge for user
poolCLICreationOutput :: PoolInfo -> String -> String -> Integer -> String -> String
poolCLICreationOutput pi@PoolInfo{..} poolAddressWithStaking poolAddress minUtxoValue origDatumWorkDir =
  let
    adaValue = if isAda tokenX then show initialXQty else show minUtxoValue
    tokenXValue = if isAda tokenX then "" else "+" ++ dq ++ show initialXQty ++ " " ++ show tokenX ++ dq
    tokenYValue = "+" ++ dq ++ show initialYQty ++ " " ++ show tokenY ++ dq
    tokenNftValue = "+" ++ dq ++ "1 " ++ show tokenNft ++ dq

    charge = floor . sqrt . fromIntegral $ (initialXQty * initialYQty)

    tokenLpValue = "+" ++ dq ++ show (lqInitQty - charge) ++ " " ++  show tokenLP ++ dq

    address = if allowStaking then poolAddressWithStaking else poolAddress

    toCardanoCliPoolValue = "--tx-out " ++ address ++ "+" ++ adaValue ++ tokenXValue ++ tokenYValue ++ tokenNftValue ++ tokenLpValue ++ " \\"

    toCardanoCliDatum = "--tx-out-inline-datum-file " ++ poolMainnetServerDatumPath origDatumWorkDir pi ++ " \\"

  in toCardanoCliPoolValue ++ ['\n'] ++ toCardanoCliDatum

--- Datum creation stuff ---

createDatumJson :: PoolInfo -> IO ()
createDatumJson pi@PoolInfo{..} = do
  let
    convertedNft = tokenInfo2CS tokenNft
    convertedX   = tokenInfo2CS tokenX
    convertedY   = tokenInfo2CS tokenY
    convertedLP  = tokenInfo2CS tokenLP

  policies <-
    if allowStaking
      then do
        mpPolicy <- getPoolMintingPolicy pi
        let
          (PlutusV2.MintingPolicyHash mpPolicyHash) = mintingPolicyHash mpPolicy
          mpCS = CurrencySymbol mpPolicyHash
        pure [mpCS]
      else pure []

  let poolConfig = PoolConfig convertedNft convertedX convertedY convertedLP 995 policies lqBound

  writeDatumToJson pi poolConfig
  pure ()

writeDatumToJson :: PoolInfo -> PoolConfig -> IO ()
writeDatumToJson pi poolDatum =
  LBS.writeFile (poolDatumPath pi) (Json.encode
    . scriptDataToJson ScriptDataJsonDetailedSchema
    . fromPlutusData
    . toData
    $ poolDatum )

testJson :: IO ()
testJson =
  print (Json.encode
    . scriptDataToJson ScriptDataJsonDetailedSchema
    . fromPlutusData
    . toData
    $ (0 :: Integer) )

--- Minting policies stuff ---

convertUplcMintingPolicy :: PoolInfo -> IO ()
convertUplcMintingPolicy pi@PoolInfo{..} =
  if allowStaking
    then do
      bytes <- BS.readFile (uplcPolicyPath pi)
      let
        shortBS = SBS.toShort bytes
        scr :: PlutusScript PlutusScriptV2
        scr = PlutusScriptSerialised shortBS
      writeFileTextEnvelope (plutusPolicyPath pi) Nothing scr
      pure ()
    else pure ()

getPoolMintingPolicy :: PoolInfo -> IO PV2.MintingPolicy
getPoolMintingPolicy pi = do
  bytes <- BS.readFile (uplcPolicyPath pi)
  let
    script = deserialise (LBS.fromStrict bytes)
  pure (PlutusV2.MintingPolicy script)

--- Staking scripts stuff ---

convertUplcStakingScript :: String -> IO ()
convertUplcStakingScript pkh = do
  bytes <- BS.readFile (uplcStakingScriptPath pkh)
  let
    shortBS = SBS.toShort bytes
    scr :: PlutusScript PlutusScriptV2
    scr = PlutusScriptSerialised shortBS
  writeFileTextEnvelope (plutusStakingScriptPath pkh) Nothing scr
  pure ()

tokenInfo2CS :: TokenInfo -> AssetClass
tokenInfo2CS TokenInfo{..} =
  let
    convertedCS = CurrencySymbol $ BuiltinByteString $ mkByteString $ T.pack curSymbol
    convertedTN = TokenName $ BuiltinByteString $ mkByteString $ T.pack tokenName
  in AssetClass (convertedCS, convertedTN)

textToPubKeyHash :: String -> PubKeyHash
textToPubKeyHash = PubKeyHash . BuiltinByteString . mkByteString . T.pack

mkByteString :: T.Text -> BS.ByteString
mkByteString input = unsafeFromEither (Hex.decode . E.encodeUtf8 $ input)

unsafeFromEither :: (Show b) => Either b a -> a
unsafeFromEither (Left err)    = Prelude.error ("Err:" ++ show err)
unsafeFromEither (Right value) = value

-- writeDataDatum2 :: FilePath -> IO ()
-- writeDataDatum2 file = do
--   LBS.writeFile file (Json.encode
--     . scriptDataToJson ScriptDataJsonDetailedSchema
--     . fromPlutusData
--     . toData
--     $ (poolDatum) )

-- tests = testGroup "SubmitApi"
--   [ buildTxBodyTests
--   , buildTxBodyContentTests 
--   , buildBalancedTxTests 
--   ]