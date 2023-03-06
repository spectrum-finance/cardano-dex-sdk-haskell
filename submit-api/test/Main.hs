{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Data.Text.Encoding as E

import Test.Tasty
import Test.Tasty.HUnit
import PlutusTx.Builtins.Internal
import PlutusTx
import ErgoDex.Contracts.Pool
import Data.ByteString.Char8 as Char8
import qualified Data.ByteString.Base16  as Hex
import qualified Data.Text.Encoding      as T
import qualified Data.ByteString.Short  as SBS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString as BS
import Codec.Serialise (serialise, deserialise)
import Data.Aeson as Json ( encode )
import qualified Data.Text.Encoding      as E
import qualified Data.ByteString.Base16  as Hex
import Plutus.Script.Utils.V2.Scripts
import qualified Data.Text as T
import Plutus.V1.Ledger.Value (AssetClass(..), assetClassValueOf, flattenValue, CurrencySymbol(..), TokenName(..))
import Plutus.V1.Ledger.Api
import ErgoDex.PValidators
import Cardano.Api (writeFileTextEnvelope, Error(displayError), ScriptDataJsonSchema (ScriptDataJsonDetailedSchema))
import Cardano.Api.Shelley (PlutusScript (..), PlutusScriptV2)
import qualified Plutus.V1.Ledger.Scripts as Plutus
import qualified Plutus.V2.Ledger.Api as PlutusV2
import qualified Cardano.Api as C
import qualified Plutus.Script.Utils.V2.Address  as PV2
import Cardano.Api.Shelley ( fromPlutusData )
import WalletAPI.TrustStore (importTrustStoreFromCardano, SecretFile (SecretFile), KeyPass (KeyPass))
import qualified Plutus.V1.Ledger.Value as Ada
import qualified Data.Map
import Ouroboros.Consensus.HardFork.Combinator.State (match)

import qualified Cardano.Api as C
import WalletAPI.TrustStore (importTrustStoreFromCardano, SecretFile (SecretFile), KeyPass (KeyPass))
import ErgoDex.Contracts.Proxy.Order (OrderRedeemer(OrderRedeemer), OrderAction (Refund))

main :: IO ()
main = do
  print test
 -- getUplc "/home/bromel/projects/cardano-dex-contracts/cardano-dex-contracts-offchain/Policy"
  -- importTrustStoreFromCardano @_ @C.PaymentKey C.AsPaymentKey (SecretFile "/home/bromel/projects/cardano-dex-sdk-haskell/submit-api/wallet1TS.json") "/home/bromel/projects/cardano-dex-sdk-haskell/submit-api/wallet1.skey" (KeyPass "secret")
  -- translate `traverse` toTranslate
  -- swap <- swapValidator 
  -- deposit <- depositValidator
  -- redeem <- redeemValidator
  -- pool <- poolValidator
  -- print (PV2.mkValidatorAddress swap)
  -- print (PV2.mkValidatorAddress deposit)
  -- print (PV2.mkValidatorAddress redeem)
  -- print (PV2.mkValidatorAddress pool)
  -- let shortBS = Hex.encode $ LBS.toStrict $ serialise (toData poolDatum)
  -- print shortBS
  pure ()
  -- pscript <- poolValidator
  -- let 
  --   shortBS = SBS.toShort . LBS.toStrict $ serialise (unValidatorScript pscript)
  --   scr :: PlutusScript PlutusScriptV2
  --   scr = PlutusScriptSerialised shortBS
  -- writeFileTextEnvelope "/home/bromel/projects/cardano-dex-sdk-haskell/submit-api/pool.plutus" Nothing scr
  -- swapInstance
  -- depositInstance
  -- redeemInstance
  -- writeDataDatum2 "/home/bromel/projects/cardano-dex-sdk-haskell/submit-api/poolDatum.json"
  -- print datumStr
  --defaultMain tests

-- getUplc uplcPath = do
--   uplcFilesNames <- listDirectory uplcPath
--   let
--     uplcFiles = (\fileName -> uplcPath ++ "/" ++ fileName) `fmap` uplcFilesNames
--   let
--     uplcWithPlutus = (\uplcPath -> (uplcPath, replaceExtension uplcPath ".script")) `fmap` uplcFiles
--   translate `traverse` uplcWithPlutus
--   let
--     plutusScritps = Prelude.fst `fmap` uplcWithPlutus
--   policies <- getPolicy `traverse` plutusScritps
--   let
--     policiesNames = takeBaseName `fmap` plutusScritps
--   let
--     policiesWithName = policies `Prelude.zip` policiesNames
--   testList <- createStr `traverse` policiesWithName
--   collectPolicies policiesWithName
--   print testList
--   pure ()

-- translate (from, to) = do
--   bytes <- BS.readFile from
--   let
--     shortBS = SBS.toShort bytes
--     scr :: PlutusScript PlutusScriptV2
--     scr = PlutusScriptSerialised shortBS
--   writeFileTextEnvelope to Nothing scr
--   pure ()

-- getPolicy path = do
--   bytes <- BS.readFile path
--   let
--     script = deserialise (LBS.fromStrict bytes)
--   pure (PlutusV2.MintingPolicy script)

-- collectPolicies :: [(MintingPolicy, [Char])] -> IO [()]
-- collectPolicies initList = do
--   let
--     firstMap = Prelude.foldl (\ accMap (mintPolicy, tName) ->
--       let
--         key = Prelude.concat . Prelude.init $ Split.splitOn "_" tName
--       in Data.Map.insertWith (++) key [(mintPolicy, tName)] accMap) Data.Map.empty initList
--     secondMap = parse `fmap` Data.Map.elems firstMap
--     thirdMap = (\(name, (lp, nft)) -> (name, (lp, nft, ("065270479316f1d92e00f7f9f095ebeaac9d009c878dc35ce36d3404", name :: String)))) `fmap` secondMap
--   writeDatum `traverse` thirdMap

-- writeDatum :: ([Char], ((MintingPolicy, String), (MintingPolicy, String), (String, String))) -> IO ()
-- writeDatum (path, ((lpPolicy, lpName), (nftPolicy, nftTn), (tokenCS, tokenName))) = do
--   lpAC <- createAssetClass (lpPolicy, lpName)
--   nftAC <- createAssetClass (nftPolicy, nftTn)
--   let
--     tCs = CurrencySymbol $ BuiltinByteString $ mkByteString $ T.pack tokenCS
--   let
--     tTn = TokenName $ BuiltinByteString $ E.encodeUtf8 $ T.pack tokenName
--   let
--     tAC = AssetClass (tCs, tTn)
--     adaAC = AssetClass (Ada.adaSymbol, Ada.adaToken)
--     datum = PoolConfig nftAC tAC adaAC lpAC 995
--   lpName <- createName (lpPolicy, lpName)
--   nftName <- createName (nftPolicy, nftTn)
--   print ("+\"1 " ++ nftName ++ "\"+\"9223372036844775807 " ++ lpName ++ "\"+\"10000000 " ++ tokenCS ++ "." ++ tokenName ++ "\" \\ ")
--   print ("--tx-out-inline-datum-file " ++ path ++ ".json" ++ "\\")
--   writeDataDatumPipe datum (path ++ ".json")

-- parse :: [(MintingPolicy, String)] -> ([Char], ((MintingPolicy, String), (MintingPolicy, String)))
-- parse ((somePolicy1, someTn1):(somePolicy2, someTn2):[]) = 
--   let
--     tokenName = Prelude.head $ Split.splitOn "_" someTn1
--   in if ("NFT" `isInfixOf` Char8.pack someTn1)
--     then (tokenName, ((somePolicy2, someTn2), (somePolicy1, someTn1)))
--     else (tokenName, ((somePolicy1, someTn1), (somePolicy2, someTn2)))
-- parse _ = undefined 

-- createName :: (MintingPolicy, [Char]) -> IO [Char]
-- createName (policy, tn) = do
--   let
--     (PlutusV2.MintingPolicyHash bbsHash) = mintingPolicyHash policy
--     bsHash = fromBuiltin bbsHash
--   let
--     cs = Hex.encode bsHash
--   let
--     tokenName = Hex.encode (Char8.pack tn)
--   pure (Char8.unpack cs ++ "." ++ Char8.unpack tokenName)

-- createAssetClass :: (MintingPolicy, [Char]) -> IO AssetClass
-- createAssetClass (policy, tn) = do
--   let
--     (PlutusV2.MintingPolicyHash bbsHash) = mintingPolicyHash policy
--     bsHash = fromBuiltin bbsHash
--   let
--     cs = CurrencySymbol $ bbsHash
--     tokenName = TokenName $ BuiltinByteString $ (Char8.pack tn)
--   pure $ AssetClass (cs, tokenName)

-- createStr :: (MintingPolicy, [Char]) -> IO [Char]
-- createStr (policy, tn) = do
--   if ("NFT" `isInfixOf` Char8.pack tn)
--     then do
--       csAndTn1 <- createName (policy, tn)
--       print csAndTn1
--       pure $ "1 " ++ csAndTn1
--     else do
--       csAndTn2 <- createName (policy, tn)
--       print csAndTn2
--       pure $ "9223372036854775807 " ++ csAndTn2

-- swapInstance = do
--   pscript <- swapValidator
--   let
--     shortBS = SBS.toShort . LBS.toStrict $ serialise (unValidatorScript pscript)
--     scr :: PlutusScript PlutusScriptV2
--     scr = PlutusScriptSerialised shortBS
--   writeFileTextEnvelope "/home/bromel/projects/cardano-dex-sdk-haskell/submit-api/swap.plutus" Nothing scr

-- depositInstance = do
--   pscript <- depositValidator
--   let
--     shortBS = SBS.toShort . LBS.toStrict $ serialise (unValidatorScript pscript)
--     scr :: PlutusScript PlutusScriptV2
--     scr = PlutusScriptSerialised shortBS
--   writeFileTextEnvelope "/home/bromel/projects/cardano-dex-sdk-haskell/submit-api/deposit.plutus" Nothing scr

-- redeemInstance = do
--   pscript <- redeemValidator
--   let
--     shortBS = SBS.toShort . LBS.toStrict $ serialise (unValidatorScript pscript)
--     scr :: PlutusScript PlutusScriptV2
--     scr = PlutusScriptSerialised shortBS
--   writeFileTextEnvelope "/home/bromel/projects/cardano-dex-sdk-haskell/submit-api/redeem.plutus" Nothing scr

-- spectrumTokenA = TokenName $ BuiltinByteString $ mkByteString $ T.pack "6e65775f737065637472756d5f746f6b656e5f61"
-- spectrumTokenB = TokenName $ BuiltinByteString $ mkByteString $ T.pack "6e65775f737065637472756d5f746f6b656e5f62"
-- spectrumTokenNFT = TokenName $ BuiltinByteString $ mkByteString $ T.pack "6e65775f737065637472756d5f746f6b656e5f6e6674"
-- spectrumTokenLP = TokenName $ BuiltinByteString $ mkByteString $ T.pack "6e65775f737065637472756d5f746f6b656e5f6c70"

-- mintPolicy = CurrencySymbol $ BuiltinByteString $ mkByteString $ T.pack "065270479316f1d92e00f7f9f095ebeaac9d009c878dc35ce36d3404"

-- spectrumTokenAClass = mkAssetClass mintPolicy spectrumTokenA
-- spectrumTokenBClass = mkAssetClass mintPolicy spectrumTokenB
-- spectrumTokenNFTClass = mkAssetClass mintPolicy spectrumTokenNFT
-- spectrumTokenLPClass = mkAssetClass mintPolicy spectrumTokenLP

-- poolDatum = PoolConfig spectrumTokenNFTClass spectrumTokenAClass spectrumTokenBClass spectrumTokenLPClass 995

orderRedeemer = OrderRedeemer 0 0 0 Refund

-- poolDatumData = toData poolDatum

datumStr = (T.decodeUtf8 . Hex.encode $ (LBS.toStrict $ serialise $ toData $ orderRedeemer))

test = do
  let 
    utf8encoded = T.encodeUtf8 $ "d8799fd8799f581c047027c6005f863e573ef821b97d6b3980e6febb06c2b95731403a2a5247454e53745f537065637472756d5f4e4654ffd8799f581c065270479316f1d92e00f7f9f095ebeaac9d009c878dc35ce36d34044547454e5374ffd8799f4040ffd8799f581c0b573d9b75c7906a778260fa3a5bfa05f32294b878bce0029fa071ee5147454e53745f537065637472756d5f4c71ff1903e3ff"
  decoded <- Hex.decode utf8encoded
  let
    test = (fromData . deserialise $ LBS.fromStrict decoded) :: Maybe PoolConfig
  pure (show decoded)

mkByteString :: T.Text -> BS.ByteString
mkByteString input = unsafeFromEither (Hex.decode . E.encodeUtf8 $ input)

-- mkAssetClass :: CurrencySymbol -> TokenName -> AssetClass
-- mkAssetClass cs tn = AssetClass (cs, tn)

unsafeFromEither :: (Show b) => Either b a -> a
unsafeFromEither (Left err)    = Prelude.error ("Err:" ++ show err)
unsafeFromEither (Right value) = value

-- jsonDatum = Json.encode
--     . scriptDataToJson ScriptDataJsonDetailedSchema
--     . fromPlutusData
--     . toData
--     $ (poolDatum)

-- writeDataDatum2 :: FilePath -> IO ()
-- writeDataDatum2 file = do
--   LBS.writeFile file (Json.encode
--     . scriptDataToJson ScriptDataJsonDetailedSchema
--     . fromPlutusData
--     . toData
--     $ (poolDatum) )

-- writeDataDatumPipe :: PoolConfig -> FilePath -> IO ()
-- writeDataDatumPipe datum2write file = do
--   LBS.writeFile file (Json.encode
--     . scriptDataToJson ScriptDataJsonDetailedSchema
--     . fromPlutusData
--     . toData
--     $ (datum2write) )

-- tests = testGroup "SubmitApi"
--   [ buildTxBodyTests
--   , buildTxBodyContentTests 
--   , buildBalancedTxTests 
--   ]
