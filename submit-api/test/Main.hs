{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RankNTypes #-}
module Main where

import qualified Data.Text.Encoding as E

import Test.Tasty
import Test.Tasty.HUnit
import PlutusTx.Builtins.Internal hiding (fst)
import PlutusTx
import ErgoDex.Contracts.Pool
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base16  as Hex
import qualified Plutus.V2.Ledger.Contexts as PV2L
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
import Cardano.CLI.Shelley.Run.Transaction
import qualified Ledger                      as P
import Cardano.Api.SerialiseTextEnvelope
import Cardano.Api (Error(displayError), ScriptDataJsonSchema (ScriptDataJsonDetailedSchema), InAnyCardanoEra (InAnyCardanoEra), EraInMode (BabbageEraInCardanoMode), IsShelleyBasedEra)
import Cardano.Api.Shelley (PlutusScript (..), PlutusScriptV2, serialiseToRawBytes, TxInMode (TxInMode))
import qualified Plutus.V1.Ledger.Scripts as Plutus
import qualified Cardano.Api as C
import qualified Plutus.Script.Utils.V2.Address  as PV2
import Cardano.Api (scriptDataToJson)
import           Data.ByteArray.Encoding (Base(..), convertToBase)
import Cardano.Api.Shelley ( fromPlutusData )
import WalletAPI.TrustStore (importTrustStoreFromCardano, SecretFile (SecretFile), KeyPass (KeyPass), mkTrustStore)
import qualified Ledger as PV2
import CardanoTx.Address (readShellyAddress)
import WalletAPI.Vault (Vault (getPaymentKeyHash), mkVault)
import qualified Explorer.Types as Explorer
import qualified Plutus.V1.Ledger.Api as P
import Cardano.Ledger.Alonzo.Data (Data(..))
import qualified Plutus.V1.Ledger.Bytes as Data
import ErgoDex.Contracts.Proxy.Deposit (DepositConfig(..))
import Plutus.V2.Ledger.Tx (OutputDatum(..))
import Ledger.Ada              (lovelaceValueOf)
import           Ledger.Value        (assetClassValue)
import qualified PlutusTx.AssocMap  as Map
import qualified Plutus.V1.Ledger.Interval as Interval
import ErgoDex.Contracts.Proxy.Order (OrderRedeemer(OrderRedeemer), OrderAction (Refund))
import Control.Monad.IO.Class (MonadIO(liftIO))
import RIO (lift, (&))
import Control.Monad.Trans.Except (runExceptT)


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

mintingPolicyNFTNamePostfix = "_mintingPolicyNFT"
mintingPolicyLQNamePostfix = "_mintingPolicyLQ"

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

uplcPolicyLqPath :: PoolInfo -> String
uplcPolicyLqPath PoolInfo{..} = workDir ++ name ++ mintingPolicyLQNamePostfix ++ uplcExtension

uplcPolicyNftPath :: PoolInfo -> String
uplcPolicyNftPath PoolInfo{..} = workDir ++ name ++ mintingPolicyNFTNamePostfix ++ uplcExtension

plutusPolicyPath :: PoolInfo -> String
plutusPolicyPath PoolInfo{..} = workDir ++ name ++ mintingPolicyNamePostfix ++ plutusExtension

plutusNftPolicyPath :: PoolInfo -> String
plutusNftPolicyPath PoolInfo{..} = workDir ++ name ++ mintingPolicyNFTNamePostfix ++ plutusExtension

plutusLqPolicyPath :: PoolInfo -> String
plutusLqPolicyPath PoolInfo{..} = workDir ++ name ++ mintingPolicyLQNamePostfix ++ plutusExtension

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
  test123

txFile :: IO ()
txFile = do
  txFinal <- runExceptT $ readFileTx "/home/bromel/projects/cardano-dex-sdk-haskell/submit-api/test/txNormal.signed"
  liftIO $ print (show (eraseLeft txFinal))
  pure ()

eraseRight :: Either a b -> Either a ()
eraseRight (Right _) = Right ()
eraseRight (Left l)  = Left l

eraseLeft :: Either a b -> Either () b
eraseLeft (Right l) = Right l
eraseLeft (Left _)  = Left ()

test123 = do
  print $ "test"
  print $ readShellyAddress "addr1wyqnt3mp3fc75mseaw74j2zxz4l3rj8uaujp20cuz7jva6q2crsut"
  let

    stablePkh :: P.PubKeyHash
    stablePkh = PubKeyHash $ BuiltinByteString $ mkByteString  "c8bf2748d61cf612fccc93287e475779ecd2c89f3e1b06da3ac4aa19"

    testAddr = P.pubKeyHashAddress (P.PaymentPubKeyHash stablePkh) Nothing

  print $ "Test pkh:" ++ show testAddr

  let
    wallet1PubKeyHash = "a78c50e7b7c4ebff6881701d3ae48198dcdbab1b731d77139e33f3d0"
    wallet2PubKeyHash = "6b4f0eace88f760261eddd8495bf4b8e3ae9743e7b65674deb90885d"
    wallet3PubKeyHash = "add49ae8756c1f76e69ef87f598a8e6ad1eff47deab073cc979ad132"

    mintingSignatures   = [wallet1PubKeyHash, wallet2PubKeyHash, wallet3PubKeyHash]

    signaturesThreshold = 2

    lqQty = 9223372036854775807

    tunapool = PoolInfo
      { name     = "tunaPool"
      , tokenX   = adaTokenInfo
      , tokenY   = TokenInfo "279f842c33eed9054b9e3c70cd6a3b32298259c24b78b895cb41d91a" "54554e41"
      , tokenNft = TokenInfo "dd061b480daddd9a833d2477c791356be4e134a433e19df7eb18be10" "4f534f43494554595f4144415f4e4654"
      , tokenLP  = TokenInfo "c44de4596c7f4d600b631fab7ef1363331168463d4229cbc75ca1889" "4b534f43494554595f5f4c51"
      , lqBound  = 0
      , authKeys = [wallet1PubKeyHash, wallet2PubKeyHash, wallet3PubKeyHash]
      , threshold = signaturesThreshold
      , initialXQty = 50000000
      , initialYQty = 16666666667
      , allowStaking = True
      }
    pools = [tunapool]

  -- Step 1. Converting uplc produced by cardano-contracts onchain repo to plutus

  convertUplcMintingPolicy `traverse` pools

  -- printLpPolicy `traverse` pools
  -- printNftPolicy `traverse` pools

  -- Step 1.5 (optional)

  -- convertUplcStakingScript wallet2PubKeyHash

  -- Step 2. Datums creation. Will produce output for cardano-cli. Also necessary to copy all datums from test to mainnet machine

  --createDatumJson `traverse` pools

  -- Step 3. Require manual steps for creation staking certs
  -- Also we cannot retrive original min utxo value for inline datums. So, set it manually
  -- More Also, we cannot determine change. So, set it manually too

  -- let
  --   poolAddressWithStaking = "addr1x94ec3t25egvhqy2n265xfhq882jxhkknurfe9ny4rl9k6dj764lvrxdayh2ux30fl0ktuh27csgmpevdu89jlxppvrst84slu"
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

  print $ readShellyAddress "stake17y4jylfjtqmnpjz4v3hwnl4etw72ghx24c6aj9xjut7af2gx5e8ca"

  -- print $ readShellyAddress "addr1vxkafxhgw4kp7ahxnmu87kv23e4drml50h4tqu7vj7ddzvs03pqaf"

  -- let testData = "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"
  --     plutusData = Data.from testData

  pool <- poolValidator
  print (PV2.mkValidatorAddress pool)
  -- swap <- swapValidator
  -- print (PV2.mkValidatorAddress swap)
  -- deposit <- depositValidator
  -- print (PV2.mkValidatorAddress deposit)
  -- redeem <- redeemValidator 
  -- print (PV2.mkValidatorAddress redeem)

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

  let poolConfig = PoolConfig convertedNft convertedX convertedY convertedLP 997 policies lqBound

  writeDatumToJson pi poolConfig
  pure ()

writeDatumToJson :: PoolInfo -> PoolConfig -> IO ()
writeDatumToJson pi poolDatum =
  LBS.writeFile (poolDatumPath pi) (Json.encode
    . scriptDataToJson ScriptDataJsonDetailedSchema
    . fromPlutusData
    . toData
    $ poolDatum )

--- Minting policies stuff ---

convertUplcMintingPolicy :: PoolInfo -> IO ()
convertUplcMintingPolicy pi@PoolInfo{..} =
  if allowStaking
    then do
      -- bytes <- BS.readFile (uplcPolicyPath pi)
      -- let
      --   shortBS = SBS.toShort bytes
      --   scr :: PlutusScript PlutusScriptV2
      --   scr = PlutusScriptSerialised shortBS
      -- writeFileTextEnvelope (plutusPolicyPath pi) Nothing scr
      lqBytes <- BS.readFile "/home/bromel/test-mainnet-pools/spfMinting.uplc"
      let
        shortLqBS = SBS.toShort lqBytes
        lqscr :: PlutusScript PlutusScriptV2
        lqscr = PlutusScriptSerialised shortLqBS
        script = deserialise (LBS.fromStrict lqBytes)
        policy = (PlutusV2.MintingPolicy script)
      writeFileTextEnvelope "/home/bromel/test-mainnet-pools/spfMinting.plutus" Nothing lqscr
      let
        (PlutusV2.MintingPolicyHash mpPolicyHash) = mintingPolicyHash policy
        mpCS = CurrencySymbol mpPolicyHash
      print $ "lq:" ++ show mpCS
      -- nftBytes <- BS.readFile (uplcPolicyNftPath pi)
      -- lqBytes <- BS.readFile (uplcPolicyLqPath pi)
      -- let
      --   shortLqBS = SBS.toShort lqBytes
      --   lqscr :: PlutusScript PlutusScriptV2
      --   lqscr = PlutusScriptSerialised shortLqBS
      -- writeFileTextEnvelope (plutusLqPolicyPath pi) Nothing lqscr
      -- nftBytes <- BS.readFile (uplcPolicyNftPath pi)
      -- let
      --   nftshortBS = SBS.toShort nftBytes
      --   nftscr :: PlutusScript PlutusScriptV2
      --   nftscr = PlutusScriptSerialised nftshortBS
      -- writeFileTextEnvelope (plutusNftPolicyPath pi) Nothing nftscr
      pure ()
    else pure ()

getPoolMintingPolicy :: PoolInfo -> IO PV2.MintingPolicy
getPoolMintingPolicy pi = do
  bytes <- BS.readFile (uplcPolicyPath pi)
  let
    script = deserialise (LBS.fromStrict bytes)
  pure (PlutusV2.MintingPolicy script)

printLpPolicy :: PoolInfo -> IO ()
printLpPolicy pi = do
  policy <- getPoolLqMintingPolicy pi
  let
    (PlutusV2.MintingPolicyHash mpPolicyHash) = mintingPolicyHash policy
    mpCS = CurrencySymbol mpPolicyHash
  print $ "lq:" ++ show mpCS
  print mpCS

printNftPolicy :: PoolInfo -> IO ()
printNftPolicy pi = do
  policy <- getPoolNftMintingPolicy pi
  let
    (PlutusV2.MintingPolicyHash mpPolicyHash) = mintingPolicyHash policy
    mpCS = CurrencySymbol mpPolicyHash
  print $ "nft:" ++ show mpCS
  print mpCS

getPoolNftMintingPolicy :: PoolInfo -> IO PV2.MintingPolicy
getPoolNftMintingPolicy pi = do
  bytes <- BS.readFile (uplcPolicyNftPath pi)
  let
    script = deserialise (LBS.fromStrict bytes)
  pure (PlutusV2.MintingPolicy script)

getPoolLqMintingPolicy :: PoolInfo -> IO PV2.MintingPolicy
getPoolLqMintingPolicy pi = do
  bytes <- BS.readFile (uplcPolicyLqPath pi)
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