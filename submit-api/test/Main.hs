{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RankNTypes #-}
module Main where

import qualified Data.Text.Encoding as E

import Test.Tasty
import Test.Tasty.HUnit
import qualified Data.Aeson as Aeson
import PlutusTx.Builtins.Internal hiding (fst)
import qualified Cardano.Api as Api
import qualified Data.Either.Combinators as EC
import qualified Ledger                     as P
import qualified PlutusTx.Builtins.Internal as BI
import PlutusTx
import ErgoDex.Contracts.Pool
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base16  as Hex
import qualified Plutus.V2.Ledger.Contexts as PV2L
import qualified Data.Text.Encoding      as T
import qualified Data.ByteString.Short  as SBS
import qualified Data.ByteString.Lazy as LBS
import Codec.Serialise (serialise, deserialise)
import Data.Aeson as Json ( encode, decode, FromJSON (parseJSON) )
import qualified Data.Text.Encoding      as E
import qualified Data.ByteString.Base16  as Hex
import Plutus.Script.Utils.V2.Scripts
import qualified Plutus.V2.Ledger.Api as PlutusV2
import qualified Data.Text as T
import Plutus.V1.Ledger.Value (AssetClass(..), assetClassValueOf, flattenValue, CurrencySymbol(..), TokenName(..))
import Plutus.V1.Ledger.Api
import ErgoDex.PValidators
import Cardano.CLI.Shelley.Run.Transaction
import Cardano.Api (writeFileTextEnvelope, Error(displayError), ScriptDataJsonSchema (ScriptDataJsonDetailedSchema), InAnyCardanoEra (InAnyCardanoEra), EraInMode (BabbageEraInCardanoMode), IsShelleyBasedEra)
import Cardano.Api.Shelley (PlutusScript (..), PlutusScriptV2, serialiseToRawBytes, TxInMode (TxInMode), toPlutusData)
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
import Explorer.Models (FullTxOut)
import ErgoDex.Contracts.Proxy.Swap (SwapConfig(SwapConfig))


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

mintingPolicyNFTNamePostfix = "_mintingPolicyNFT"
mintingPolicyLQNamePostfix = "_mintingPolicyLQ"

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

uplcPolicyLqPath :: PoolInfo -> String
uplcPolicyLqPath PoolInfo{..} = workDir ++ name ++ mintingPolicyLQNamePostfix ++ uplcExtension

uplcPolicyNftPath :: PoolInfo -> String
uplcPolicyNftPath PoolInfo{..} = workDir ++ name ++ mintingPolicyNFTNamePostfix ++ uplcExtension

plutusLqPolicyPath :: PoolInfo -> String
plutusLqPolicyPath PoolInfo{..} = workDir ++ name ++ mintingPolicyLQNamePostfix ++ plutusExtension

plutusNftPolicyPath :: PoolInfo -> String
plutusNftPolicyPath PoolInfo{..} = workDir ++ name ++ mintingPolicyNFTNamePostfix ++ plutusExtension

plutusLqPolicyPathWithDir :: PoolInfo -> String -> String
plutusLqPolicyPathWithDir PoolInfo{..} dir = dir ++ name ++ mintingPolicyLQNamePostfix ++ plutusExtension

plutusNftPolicyPathwithDir :: PoolInfo -> String -> String
plutusNftPolicyPathwithDir PoolInfo{..} dir = dir ++ name ++ mintingPolicyNFTNamePostfix ++ plutusExtension

uplcStakingScriptPath :: String -> String
uplcStakingScriptPath pkh = workDir ++ pkh ++ stakingScriptNamePostfix ++ uplcExtension

plutusStakingScriptPath ::  String -> String
plutusStakingScriptPath pkh = workDir ++ pkh ++ stakingScriptNamePostfix ++ plutusExtension

poolDatumPath :: PoolInfo -> String
poolDatumPath PoolInfo{..} = workDir ++ name ++ poolDatumPostfix ++ jsonExtension

poolMainnetServerDatumPath :: String -> PoolInfo -> String
poolMainnetServerDatumPath mainnetWorkDir PoolInfo{..} = mainnetWorkDir ++ name ++ poolDatumPostfix ++ jsonExtension

adanftPostfix = "5F4144415F4E4654"
adalqPostfix = "5F4144415F4C51"

wallet1PubKeyHash = "a78c50e7b7c4ebff6881701d3ae48198dcdbab1b731d77139e33f3d0"
wallet2PubKeyHash = "6b4f0eace88f760261eddd8495bf4b8e3ae9743e7b65674deb90885d"
wallet3PubKeyHash = "add49ae8756c1f76e69ef87f598a8e6ad1eff47deab073cc979ad132"

mintingSignatures   = [wallet1PubKeyHash, wallet2PubKeyHash, wallet3PubKeyHash]

signaturesThreshold = 2

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

test3 = do
  deposit <- depositValidator

  let
    depositAddress = PV2.mkValidatorAddress deposit

    inputAda  = lovelaceValueOf 11929173

    snekAssetClass = tokenInfo2CS $ TokenInfo "279c909f348e533da5808898f87f9a14bb2c3dfbbacccd631d927a3f" "534e454b"
    inputSnek = assetClassValue snekAssetClass 10000

    poolNft = tokenInfo2CS $ TokenInfo "4a27465112a39464e6dd5ee470c552ebb3cb42925d5ec04014967908" "534E454B5F4144415F4E4654"
    poolLp  = tokenInfo2CS $ TokenInfo "7bddf2c27f257eeeef3e892758b479e09c89a73642499797f2a97f3c" "534E454B5F4144415F4C51"

    inputDatum = DepositConfig
      { poolNft = poolNft
      , tokenA = tokenInfo2CS adaTokenInfo
      , tokenB = snekAssetClass
      , tokenLp = poolLp
      , exFee = 1500000
      , rewardPkh = PubKeyHash $ BuiltinByteString $ mkByteString $ T.pack "719bee424a97b58b3dca88fe5da6feac6494aa7226f975f3506c5b25"
      , stakePkh = Just $ PubKeyHash $ BuiltinByteString $ mkByteString $ T.pack "7846f6bb07f5b2825885e4502679e699b4e60a0c4609a46bc35454cd"
      , collateralAda = 0
      }

    refundInput = PV2L.TxInInfo {
      txInInfoOutRef = TxOutRef {
        txOutRefId  = TxId $ BuiltinByteString $ mkByteString $ T.pack "818804916028d33eef09eb2c5dac47d2c38094eeba21daac65c1627afd82884d",
        txOutRefIdx = 0
      },
      txInInfoResolved = PlutusV2.TxOut {
        txOutAddress = depositAddress,
        txOutValue   = inputAda <> inputSnek,
        txOutDatum   = OutputDatum $ Datum $ toBuiltinData inputDatum,
        txOutReferenceScript = Just $ scriptHash (unValidatorScript deposit)
      }
    }

    unknownReferenceInput = refundInput

    depositRefInputAda = lovelaceValueOf 1226634

    depositReferenceInput = PV2L.TxInInfo {
      txInInfoOutRef = TxOutRef {
        txOutRefId  = TxId $ BuiltinByteString $ mkByteString $ T.pack "fc9e99fd12a13a137725da61e57a410e36747d513b965993d92c32c67df9259a",
        txOutRefIdx = 0
      },
      txInInfoResolved = PlutusV2.TxOut {
        txOutAddress = Address
          (PubKeyCredential $ PubKeyHash $ BuiltinByteString $ mkByteString $ T.pack "a6e1973e53af80c473cafb288235864f66240d305d9ce9df992125ea")
          (Just $ StakingHash $ PubKeyCredential $ PubKeyHash $ BuiltinByteString $ mkByteString $ T.pack "3f70ef0595dbc750d6575d814af8da0cdb53e778dae4895e85ef239e"),
        txOutValue   = depositRefInputAda,
        txOutDatum   = NoOutputDatum,
        txOutReferenceScript = Nothing
      }
    }

    userTxOutAda = lovelaceValueOf 10452541

    userTxOut = PlutusV2.TxOut {
        txOutAddress = Address
          (PubKeyCredential $ PubKeyHash $ BuiltinByteString $ mkByteString $ T.pack "21bcdaa800d642aa94d62ab43524de3481a7790b69172e7e3ef882ec")
          (Just $ StakingHash $ PubKeyCredential $ PubKeyHash $ BuiltinByteString $ mkByteString $ T.pack "7846f6bb07f5b2825885e4502679e699b4e60a0c4609a46bc35454cd"),
        txOutValue   = userTxOutAda <> inputSnek,
        txOutDatum   = NoOutputDatum,
        txOutReferenceScript = Nothing
      }

    spendingRef = TxOutRef {
        txOutRefId  = TxId $ BuiltinByteString $ mkByteString $ T.pack "818804916028d33eef09eb2c5dac47d2c38094eeba21daac65c1627afd82884d",
        txOutRefIdx = 0
      }

    orderRedeemer = toBuiltinData $ OrderRedeemer 0 0 0 Refund

    txId = TxId $ BuiltinByteString $ mkByteString $ T.pack "349709cb602d3ae5405e8fba4888c4f31706345c183014efe1b5388447aadca8"

    ctx = PV2L.TxInfo
      { txInfoInputs          = [refundInput] -- ^ Transaction inputs
      , txInfoReferenceInputs = [unknownReferenceInput, depositReferenceInput] -- ^ Transaction reference inputs
      , txInfoOutputs         = [userTxOut] -- ^ Transaction outputs
      , txInfoFee             = lovelaceValueOf 1476632 -- ^ The fee paid by this transaction.
      , txInfoMint            = lovelaceValueOf 0 -- ^ The 'Value' minted by this transaction.
      , txInfoDCert           = [] -- ^ Digests of certificates included in this transaction
      , txInfoWdrl            = Map.empty -- ^ Withdrawals
      , txInfoValidRange      = Interval.always -- ^ The valid range for the transaction.
      , txInfoSignatories     = [
        PubKeyHash $ BuiltinByteString $ mkByteString $ T.pack "21bcdaa800d642aa94d62ab43524de3481a7790b69172e7e3ef882ec",
        PubKeyHash $ BuiltinByteString $ mkByteString $ T.pack "719bee424a97b58b3dca88fe5da6feac6494aa7226f975f3506c5b25" -- collateral signature + datum
      ] -- ^ Signatures provided with the transaction, attested that they all signed the tx
      , txInfoRedeemers       = Map.fromList [(Spending spendingRef, Redeemer orderRedeemer)]
      , txInfoData            = Map.empty
      , txInfoId              = txId
      -- ^ Hash of the pending transaction (excluding witnesses)
      }

  print depositAddress

  print $ show $ toBuiltinData ctx
  -- print $ readShellyAddress "addr1q9cehmjzf2tmtzeae2y0uhdxl6kxf992wgn0ja0n2pk9kftcgmmtkpl4k2p93p0y2qn8ne5eknnq5rzxpxjxhs652nxsqwq3mt"
  pure ()

test123 = do
  readDatumJson
  -- let
  --   trustStore = mkTrustStore @_ @C.PaymentKey C.AsPaymentKey (SecretFile "/home/bromel/projects/cardano-dex-sdk-haskell/wallet1TS.json")
  --   vault      = mkVault trustStore $ KeyPass $ T.pack "test1234" :: Vault IO

  --   mkPCred = Explorer.PaymentCred . T.decodeUtf8 . convertToBase Base16 . serialiseToRawBytes

  -- pkh <- getPaymentKeyHash vault

  -- let
  --   address = (mkPCred pkh)

  -- print address

    -- defaultMain tests
  let
    poolsInfo = 
      [ ("COPI", "b6a7467ea1deb012808ef4e87b5ff371e85f7142d7b356a40d9b42a0", "436f726e75636f70696173205b76696120436861696e506f72742e696f5d" , 0, 100000000, 869565217)
      , ("IBTC", "f66d78b4a3cb3d37afa0ec36461e51ecbde00f26c8f0a68f94b69880", "69425443", 10000000000, 100000000, 928)
      , ("MELD", "a2944573e99d2ed3055b808eaa264f0bf119e01fc6b18863067c63e4", "4d454c44", 10000000000, 100000000, 2049180328)
      , ("NTX", "edfd7a1d77bcb8b884c474bdc92a16002d1fb720e454fa6e99344479", "4e5458", 10000000000, 100000000, 699300699)
      , ("AGIX", "f43a62fdc3965df486de8a0d32fe800963589c41b38946602a0dc535", "41474958", 10000000000, 100000000, 14727540501)
      , ("cBTC", "4190b2941d9be04acc69c39739bd5acc66d60ccab480d8e20bc87e37", "63425443", 10000000000, 150000000, 163681)
      ]
    lqQty = 9223372036854775807

  -- pools = [rabbitPool, goldfishPool, rabbitFoldfishPool]

  -- bytes <- BS.readFile "/home/bromel/projects/cardano-dex-contracts/cardano-dex-contracts-offchain/Contracts/pool.uplc"
  -- let
  --   shortBS = SBS.toShort bytes
  --   scr :: PlutusScript PlutusScriptV2
  --   scr = PlutusScriptSerialised shortBS
  -- print $ scriptHash (deserialise (LBS.fromStrict bytes))

  --writeFileTextEnvelope "/home/bromel/projects/cardano-dex-sdk-haskell/submit-api/pool.plutus" Nothing scr

  -- Step 1. Converting uplc produced by cardano-contracts onchain repo to plutus

  pools <- createFirstPoolInfo `traverse` poolsInfo

  -- convertUplcMintingPolicy `traverse` pools

  -- Step 1.5 (optional)

  -- convertUplcStakingScript wallet2PubKeyHash

  -- Step 2. Datums creation. Will produce output for cardano-cli. Also necessary to copy all datums from test to mainnet machine

  -- createDatumJson `traverse` pools

  -- Step 3. Require manual steps for creation staking certs
  -- Also we cannot retrive original min utxo value for inline datums. So, set it manually
  -- More Also, we cannot determine change. So, set it manually too

  let
    poolAddressWithStaking = "addr1x94ec3t25egvhqy2n265xfhq882jxhkknurfe9ny4rl9k6dj764lvrxdayh2ux30fl0ktuh27csgmpevdu89jlxppvrst84slu"
    poolAddress            = "addr1w9cgs59t2hr5sphrv4gfuzxl323akly5z57qv07hq266evsg37dfw"
    -- on mainnet machine
    origDatumWorkDir       = "/root/plutus-scripts-for-mainnet/test-pools/datums/"

    bootstrapAddressString   = ""
    bootstrapAddressVKeyPath = ""

    minUtxoValueForPool    = 3223960

    res = (\pi -> poolCLICreationOutput pi poolAddressWithStaking poolAddress minUtxoValueForPool origDatumWorkDir) `fmap` pools

    folded = foldr (\acc nextPool -> acc ++ "\n" ++ nextPool) "" res

  -- mintRes <- mintingCLICreationOutput pools

  -- putStr folded

  -- putStr mintRes

  --putStr folded

  -- end

  --print $ readShellyAddress "addr1v8g2jvkr55vsqlteuu5x0052lgj3ak0ev5vs74dyu0fgahg92dth0"

  -- print $ readShellyAddress "addr1qxupdk69sdemdx80far0tsvrydz7zj67ydzxxujmv9srj3tcgmmtkpl4k2p93p0y2qn8ne5eknnq5rzxpxjxhs652nxsy8ugdz"

  --print $ readShellyAddress "addr1x94ec3t25egvhqy2n265xfhq882jxhkknurfe9ny4rl9k6dj764lvrxdayh2ux30fl0ktuh27csgmpevdu89jlxppvrst84slu"
  --print $ readShellyAddress "addr1x8nz307k3sr60gu0e47cmajssy4fmld7u493a4xztjrll0aj764lvrxdayh2ux30fl0ktuh27csgmpevdu89jlxppvrswgxsta"

  -- let testData = "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"
  --     plutusData = Data.from testData

  -- pool <- poolValidator
  -- print (PV2.mkValidatorAddress pool)
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

getNftPolicy :: PoolInfo -> IO String
getNftPolicy pi = do
  nftMintingPolicy <- getPoolNftMintingPolicy pi
  let nftHash = scriptHash (unMintingPolicyScript nftMintingPolicy)
  pure (show nftHash)

getLqPolicy :: PoolInfo -> IO String
getLqPolicy pi = do
  lqMintingPolicy <- getPoolLQMintingPolicy pi
  let lqHash = scriptHash (unMintingPolicyScript lqMintingPolicy)
  pure (show lqHash)

--                 poolName, policy  token   minAda deployAda
createFirstPoolInfo :: (String, String, String, Integer, Integer, Integer) -> IO PoolInfo
createFirstPoolInfo (poolName, policy, base16, minAda, deployAda, token) = do
  let
    tokenBase16 = if (base16 == "436f726e75636f70696173205b76696120436861696e506f72742e696f5d") then "436F726E75636F70696173" else base16
    testPi = PoolInfo {
      name     = poolName
      , tokenX   = adaTokenInfo
      , tokenY   = TokenInfo policy base16
      , tokenNft = TokenInfo "85157c33ec50d85e920f341a808062c827b91595c1ffe7ee2b162be25b13cbee" (tokenBase16 ++ adanftPostfix)
      , tokenLP  = TokenInfo "85157c33ec50d85e920f341a808062c827b91595c1ffe7ee2b162be25b13cbee" (tokenBase16 ++ adalqPostfix)
      , lqBound  = minAda
      , authKeys = [wallet1PubKeyHash, wallet2PubKeyHash, wallet3PubKeyHash]
      , threshold = signaturesThreshold
      , initialXQty = deployAda
      , initialYQty = token
      , allowStaking = True
    }


  nft <- getNftPolicy testPi
  lq  <- getLqPolicy testPi
  pure $ PoolInfo {
    name     = poolName
    , tokenX   = adaTokenInfo
    , tokenY   = TokenInfo policy base16
    , tokenNft = TokenInfo nft (tokenBase16 ++ adanftPostfix)
    , tokenLP  = TokenInfo lq (tokenBase16 ++ adalqPostfix)
    , lqBound  = minAda
    , authKeys = [wallet1PubKeyHash, wallet2PubKeyHash, wallet3PubKeyHash]
    , threshold = signaturesThreshold
    , initialXQty = deployAda
    , initialYQty = token
    , allowStaking = True
  }

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

mintingCLICreationOutput :: [PoolInfo] -> IO String
mintingCLICreationOutput piList = do
  mintInfo <- poolInfoMint `traverse` piList
  mintScripts <- poolInfoScriptMint `traverse` piList
  let foldedMintInfo = (foldl (\acc nextInfo -> acc ++ "+" ++ nextInfo) "" mintInfo) ++ " \\"
    -- tokenYValue = "+" ++ dq ++ show initialYQty ++ " " ++ show tokenY ++ dq
    -- tokenNftValue = "+" ++ dq ++ "1 " ++ show tokenNft ++ dq

    -- charge = floor . sqrt . fromIntegral $ (initialXQty * initialYQty)

    -- tokenLpValue = "+" ++ dq ++ show (lqInitQty - charge) ++ " " ++  show tokenLP ++ dq

    -- address = if allowStaking then poolAddressWithStaking else poolAddress

    -- toCardanoCliPoolValue = "--tx-out " ++ address ++ "+" ++ adaValue ++ tokenXValue ++ tokenYValue ++ tokenNftValue ++ tokenLpValue ++ " \\"

    -- toCardanoCliDatum = "--tx-out-inline-datum-file " ++ poolMainnetServerDatumPath origDatumWorkDir pi ++ " \\"

  pure $ foldedMintInfo ++ ['\n'] ++ (concat mintScripts) -- toCardanoCliPoolValue ++ ['\n'] ++ toCardanoCliDatum

poolInfoMint :: PoolInfo -> IO String
poolInfoMint PoolInfo{..} = do
  pure $ dq ++ "1 " ++ show tokenNft ++ dq ++ "+" ++ dq ++ "9223372036854775807 " ++ show tokenLP ++ dq

poolInfoScriptMint :: PoolInfo -> IO String
poolInfoScriptMint pi@PoolInfo{..} = do
  let
    nftMint = "--mint-script-file " ++ plutusNftPolicyPathwithDir pi "/root/plutus-scripts-for-mainnet/test-pools/datums/" ++ " \\" ++ ['\n']
    nftRedeemer = "--mint-redeemer-file /root/plutus-scripts-for-mainnet/datums/unit.json" ++ " \\" ++ ['\n']
    lqMint = "--mint-script-file " ++ plutusLqPolicyPathWithDir pi "/root/plutus-scripts-for-mainnet/test-pools/datums/" ++ " \\" ++ ['\n']
    lqRedeemer = "--mint-redeemer-file /root/plutus-scripts-for-mainnet/datums/unit.json" ++ " \\" ++ ['\n']
  pure $ nftMint ++ nftRedeemer ++ lqMint ++ lqRedeemer

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

readDatumJson :: IO ()
readDatumJson = do
  let
    a = "{\"a\":123}"
    test = decode a :: Maybe FullTxOut
    rawDataM = decode "{\"fields\": [ { \"fields\": [ { \"bytes\": \"\" }, { \"bytes\": \"\" } ], \"constructor\": 0 }, { \"fields\": [ { \"bytes\": \"b34b3ea80060ace9427bda98690a73d33840e27aaa8d6edb7f0c757a\" }, { \"bytes\": \"634e455441\" } ], \"constructor\": 0 }, { \"fields\": [ { \"bytes\": \"bca5f2951474244a220f7336f5789fbf9cfbb7fe62bf225a9c99fcae\" }, { \"bytes\": \"636e6574615f6164615f6e6674\" } ], \"constructor\": 0 }, { \"int\": 997 }, { \"int\": 31250 }, { \"int\": 1 }, { \"bytes\": \"1f8cd6fa960aae53525e68d5e84880f1c889e827344730d8e830f172\" }, { \"fields\": [ { \"bytes\": \"9db7e35cb62c4bf25bd2140230bf9aff73a6319bbee02d727a871b22\" } ], \"constructor\": 0 }, { \"int\": 1000000 }, { \"int\": 48 } ], \"constructor\": 0 }" :: Maybe Aeson.Value
    jsonDataM     = rawDataM >>= (EC.rightToMaybe . Api.scriptDataFromJson Api.ScriptDataJsonDetailedSchema)
    data'         = (fmap (fromBuiltinData . BI.dataToBuiltinData . toPlutusData) jsonDataM) :: Maybe (Maybe SwapConfig)
  print test
  print data'
  pure ()
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
      lqBytes <- BS.readFile (uplcPolicyLqPath pi)
      let
        shortLqBS = SBS.toShort lqBytes
        lqscr :: PlutusScript PlutusScriptV2
        lqscr = PlutusScriptSerialised shortLqBS
      writeFileTextEnvelope (plutusLqPolicyPath pi) Nothing lqscr
      nftBytes <- BS.readFile (uplcPolicyNftPath pi)
      let
        nftshortBS = SBS.toShort nftBytes
        nftscr :: PlutusScript PlutusScriptV2
        nftscr = PlutusScriptSerialised nftshortBS
      writeFileTextEnvelope (plutusNftPolicyPath pi) Nothing nftscr
      pure ()
    else pure ()

getPoolMintingPolicy :: PoolInfo -> IO PV2.MintingPolicy
getPoolMintingPolicy pi = do
  bytes <- BS.readFile (uplcPolicyPath pi)
  let
    script = deserialise (LBS.fromStrict bytes)
  pure (PlutusV2.MintingPolicy script)

getPoolLQMintingPolicy :: PoolInfo -> IO PV2.MintingPolicy
getPoolLQMintingPolicy pi = do
  bytes <- BS.readFile (uplcPolicyLqPath pi)
  let
    script = deserialise (LBS.fromStrict bytes)
    (PlutusV2.MintingPolicyHash mpPolicyHash) = mintingPolicyHash (PlutusV2.MintingPolicy script)
    mpCS = CurrencySymbol mpPolicyHash
  pure (PlutusV2.MintingPolicy script)

getPoolNftMintingPolicy :: PoolInfo -> IO PV2.MintingPolicy
getPoolNftMintingPolicy pi = do
  bytes <- BS.readFile (uplcPolicyNftPath pi)
  let
    script = deserialise (LBS.fromStrict bytes)
    (PlutusV2.MintingPolicyHash mpPolicyHash) = mintingPolicyHash (PlutusV2.MintingPolicy script)
    mpCS = CurrencySymbol mpPolicyHash
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