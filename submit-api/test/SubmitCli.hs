module SubmitCli where

import RIO

import qualified CardanoTx.Models               as Sdk

import qualified Ledger.Ada as Ada
import Plutus.V1.Ledger.Ada    (lovelaceValueOf)
import Plutus.V1.Ledger.Value
import           Data.Functor    ((<&>))
import qualified Data.ByteString as BS

import Test.Tasty
import Test.Tasty.HUnit
import Hedgehog
import Hedgehog.Gen as Gen
import Hedgehog.Range as Range
import   qualified        Ledger as Ledger
import Gen.CardanoTx
import Spec.Network

import qualified Cardano.Api as C

import NetworkAPI.Env
import SubmitAPI.Internal.Transaction
import CardanoTx.Models
import CardanoTx.Interop as Interop
import Plutus.V1.Ledger.Tx
import qualified Cardano.Api                as C
import qualified Data.Text as T
import Cardano.Api
import PlutusTx.Builtins.Internal 
import qualified PlutusTx as PlutusTx
import qualified Plutus.V1.Ledger.Address as PAddress
import qualified Plutus.V1.Ledger.Credential as PCredential
import qualified Plutus.V1.Ledger.Crypto as PCrypto
import qualified Cardano.Api as CAddress
import Cardano.Api.Shelley
import CardanoTx.Address
import qualified Data.ByteString.Base16  as Hex
import qualified Data.Text.Encoding      as E
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Char8 as BSS
import qualified Data.ByteString.Short as SBS
import qualified Data.ByteString.Lazy  as LBS
import SubmitAPI.Config
import System.Process
import qualified Data.Aeson as Encode
import Cardano.Api (writeFileTextEnvelope, Error(displayError))

import Codec.Serialise          (serialise )

data SubmitCliService f =
  SubmitCliService
    { submit :: Sdk.TxCandidate -> f ()
    }

makeSubmitCliService :: (MonadIO f) => DefaultChangeAddress -> SubmitCliService f
makeSubmitCliService addr =
  SubmitCliService
    { submit = submit' addr
    }

submit' :: (MonadIO f) => DefaultChangeAddress -> Sdk.TxCandidate -> f ()
submit' addr candidate = do
  buildTx' <- buildTx addr candidate
  liftIO $ callCommand buildTx' 
  liftIO $ callCommand signTx'
  liftIO $ callCommand submitTx

buildTx :: (MonadIO f) => DefaultChangeAddress -> Sdk.TxCandidate -> f String
buildTx cfg Sdk.TxCandidate{..} = do
  inputs <- renderInputs txCandidateInputs
  return $ mconcat 
    [ "$CARDANO_CLI transaction build"
    , inputs
    , renderOutputs txCandidateOutputs
    , " "
    , "--change-address="
    , changeAddressToCli cfg txCandidateChangePolicy
    , " "
    , "--testnet-magic ${TESTNET_MAGIC_NUM}"
    , " "
    , "--out-file tx.build"
    , " "
    , "--alonzo-era"
    ]

signTx' :: String
signTx' =
  mconcat
    [ "$CARDANO_CLI transaction sign"
    , " "
    , "--tx-body-file tx.build"
    , " "
    , "--signing-key-file ./wallets/wallet3.skey"
    , " "
    , "--out-file tx.signed"
    ]

submitTx :: String
submitTx =
  mconcat
    [ "$CARDANO_CLI transaction submit"
    , " "
    , "--tx-file tx.signed"
    , " "
    , "--testnet-magic $TESTNET_MAGIC_NUM"
    ]

renderInputs :: (MonadIO f) => [Sdk.FullTxIn] -> f String
renderInputs inputs = do
  let
    zipped = zip [0..] inputs
  foldM renderInput mempty zipped

renderInput :: (MonadIO f) => String -> (Integer, Sdk.FullTxIn) -> f String
renderInput acc (index, Sdk.FullTxIn{fullTxInTxOut=Sdk.FullTxOut{fullTxOutRef}, fullTxInType}) = do
  let txIn = acc <> " --tx-in " <> renderTxOutRef fullTxOutRef
  txInType <- renderTxInType index fullTxInType
  return $ txIn <> txInType

renderOutputs :: [Sdk.TxOutCandidate] -> String
renderOutputs = foldl (\acc out -> acc <> renderOutput out) mempty 

renderOutput :: Sdk.TxOutCandidate -> String
renderOutput candidate =
  let
    lovelace          = Ada.getLovelace . Ada.fromValue $ Sdk.txOutCandidateValue candidate
    tokens            = flattenValue $ (Sdk.txOutCandidateValue candidate) <> (Ada.lovelaceValueOf (-lovelace))
    prettyTokens      = foldl (\acc (_, _, i) -> mconcat [ acc, "+", show i]) mempty tokens
    prettyFullTokens  = foldl prittyTokenFullValue mempty tokens
    serialisedAddress = unpackAddress (Sdk.txOutCandidateAddress candidate)
    dh                = renderDatumHash (txOutCandidateDatum candidate)
  in " --tx-out " <> serialisedAddress <> "+" <> (show lovelace) <> prettyTokens <> prettyFullTokens <> dh

renderTxInType :: (MonadIO f) => Integer -> TxInType -> f String
renderTxInType count (ConsumeScriptAddress v r d) = do
  let 
    contractName   = "contract" <> show count <> ".plutus"
    renderContract = " --tx-in-script-file " <> contractName
    script = PlutusScriptSerialised . SBS.toShort . LBS.toStrict . serialise . Ledger.unValidatorScript $ v :: PlutusScript PlutusScriptV1
  _ <- liftIO $ void $ writeFileTextEnvelope contractName Nothing script

  let 
    redeemerName   = "redeemer" <> show count <> ".json"
    renderRedeemr  = " --tx-in-redeemer-file " <> redeemerName
    redeemerToFile = BSS.unpack $ B.toStrict $ Encode.encode (scriptDataToJson ScriptDataJsonDetailedSchema $ fromPlutusData (PlutusTx.toData r))
  _ <- liftIO $ writeFile redeemerName redeemerToFile

  let
    datumName = "datum" <> show count <> ".json"
    renderDatum = " --tx-in-datum-file " <> datumName
    datumToFile = BSS.unpack $ B.toStrict $ Encode.encode (scriptDataToJson ScriptDataJsonDetailedSchema $ fromPlutusData (PlutusTx.toData d))
  _ <- liftIO $ writeFile datumName datumToFile

  pure $ renderContract <> renderRedeemr <> renderDatum

renderTxInType _ _ = return mempty
    
renderDatumHash :: OutDatum -> String
renderDatumHash out =
  case outDatumHash out of
    Just hash -> " --tx-out-datum-hash=" <> show hash
    _ -> "" 

unpackAddress :: PAddress.Address -> String
unpackAddress address =
    case (fmap (\shelly -> T.unpack $ serialiseAddress shelly) toShellyAddr) of
        Just r -> r
        _      -> ""
  where
    toShellyAddr = renderToShelleyFromAddress (C.Testnet $ C.NetworkMagic 1097911063) address

changeAddressToCli :: DefaultChangeAddress -> Maybe ChangePolicy -> String
changeAddressToCli deafultChangeAddr policy =
  case policy of
    Just (ReturnTo address) -> unpackAddress address
    _                       -> unpackAddress $ unwrapChangeAddress deafultChangeAddr

renderTxOutRef :: TxOutRef -> String
renderTxOutRef TxOutRef{..} = show txOutRefId ++ "#" ++ (show txOutRefIdx)

prittyAssetClass :: CurrencySymbol -> TokenName -> String
prittyAssetClass cs TokenName{unTokenName=(BuiltinByteString bs)} = show cs <> "." <> (T.unpack $ E.decodeUtf8 $ Hex.encode bs)

prittyTokenFullValue :: String -> (CurrencySymbol, TokenName, Integer) -> String
prittyTokenFullValue acc (cs, tn, i) = 
  mconcat 
    [ acc
    , "+"
    , "\""
    , show i
    , " "
    , prittyAssetClass cs tn
    , "\""
    ]