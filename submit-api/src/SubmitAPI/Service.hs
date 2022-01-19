module SubmitAPI.Service where

import qualified RIO.List as L
import           RIO
import qualified Data.Set              as Set
import qualified Data.ByteString.Char8 as B8
import           GHC.Natural           (naturalToInteger)
import qualified PlutusTx.AssocMap as Map
import qualified CardanoTx.Models            as Sdk
import qualified Cardano.Api                 as C
import qualified Ledger                      as P
import qualified PlutusTx.Builtins.Internal  as P
import qualified Ledger.Ada                  as P
import qualified Plutus.V1.Ledger.Credential as P
import Plutus.V1.Ledger.Api (Value(..))
import           SubmitAPI.Config
import           SubmitAPI.Internal.Transaction
import           NetworkAPI.Service             hiding (submitTx)
import qualified NetworkAPI.Service             as Network
import           NetworkAPI.Env
import           WalletAPI.Vault

data Transactions f = Transactions
  { finalizeTx :: Sdk.TxCandidate  -> f (C.Tx C.AlonzoEra)
  , submitTx   :: C.Tx C.AlonzoEra -> f ()
  }

mkSubmitService
  :: (MonadThrow f, MonadIO f)
  => Network f
  -> Vault f
  -> TxAssemblyConfig
  -> Transactions f
mkSubmitService network wallet conf = Transactions
  { finalizeTx = finalizeTx' network wallet conf
  , submitTx   = Network.submitTx network
  }

finalizeTx'
  :: (MonadThrow f, MonadIO f)
  => Network f
  -> Vault f
  -> TxAssemblyConfig
  -> Sdk.TxCandidate
  -> f (C.Tx C.AlonzoEra)
finalizeTx' Network{..} wallet@Vault{..} conf@TxAssemblyConfig{..} txc@Sdk.TxCandidate{..} = do
  sysenv      <- getSystemEnv
  collaterals <- mkCollaterals wallet sysenv conf txc
  liftIO $ print "----"
  liftIO $ print collaterals
  liftIO $ print "----"

  let
    isBalancedTx = amountIn == amountOut
      where
        amountIn =
          foldr (\ txIn acc -> Sdk.fullTxOutValue (Sdk.fullTxInTxOut txIn) <> acc) mempty (Set.elems txCandidateInputs)
        amountOut =
          foldr (\ txOut acc -> Sdk.txOutCandidateValue txOut <> acc) mempty txCandidateOutputs
  (C.BalancedTxBody txb _ _) <- case txCandidateChangePolicy of
    Just (Sdk.ReturnTo changeAddr) -> buildBalancedTx sysenv (Sdk.ChangeAddress changeAddr) collaterals txc
    _ | isBalancedTx               -> buildBalancedTx sysenv dummyAddr collaterals txc

  liftIO $ print "<3>"

  let
    requiredSigners = Set.elems txCandidateInputs >>= getPkh
      where
        getPkh Sdk.FullTxIn{fullTxInTxOut=Sdk.FullTxOut{fullTxOutAddress=P.Address (P.PubKeyCredential pkh) _}} = [pkh]
        getPkh _                                                                                                = []
  liftIO $ print "<4>"
  signers <- mapM (\pkh -> getSigningKey pkh >>= maybe (throwM $ SignerNotFound pkh) pure) requiredSigners
  liftIO $ print "<5>"
  pure $ signTx txb signers

mkCollaterals
  :: (MonadThrow f, MonadIO f)
  => Vault f
  -> SystemEnv
  -> TxAssemblyConfig
  -> Sdk.TxCandidate
  -> f (Set.Set Sdk.FullCollateralTxIn)
mkCollaterals wallet sysenv@SystemEnv{..} TxAssemblyConfig{..} txc@Sdk.TxCandidate{..} = do
  let isScriptIn Sdk.FullTxIn{fullTxInType=P.ConsumeScriptAddress {}} = True
      isScriptIn _                                                    = False

      scriptInputs = filter isScriptIn (Set.elems txCandidateInputs)

      collectCollaterals knownCollaterals = do
        let
          estimateCollateral' collaterals = do
            (C.BalancedTxBody _ _ fee) <- buildBalancedTx sysenv dummyAddr collaterals txc
            let (C.Quantity fee')  = C.lovelaceToQuantity fee
                collateralPercent' = naturalToInteger collateralPercent
            pure $ P.Lovelace $ collateralPercent' * fee' `div` 100

        collateral <- estimateCollateral' knownCollaterals
        utxos      <- selectUtxos wallet (P.toValue (P.Lovelace 20000000)) >>= maybe (throwM FailedToSatisfyCollateral) pure
        let inputsWOTokens = Set.filter onlyAdaValue utxos
        let refs = Set.map (\x -> Sdk.fullTxOutRef $ Sdk.fullTxInTxOut x) txCandidateInputs
        let filtered = Set.filter (\Sdk.FullTxOut{..}-> not (Set.member fullTxOutRef refs)) inputsWOTokens
        let collaterals = Set.fromList $ Set.elems filtered <&> Sdk.FullCollateralTxIn
        liftIO $ print "<!~~~~>!"
        liftIO $ print utxos
        liftIO $ print inputsWOTokens
        liftIO $ print refs
        liftIO $ print filtered
        liftIO $ print collaterals
        liftIO $ print "<!~~~~>!"
        collateral' <- estimateCollateral' collaterals

        if collateral' > collateral
        then collectCollaterals collaterals
        else pure collaterals

  case (scriptInputs, collateralPolicy) of
    ([], _)    -> pure mempty
    (_, Cover) -> collectCollaterals mempty  
    _          -> throwM CollateralNotAllowed

onlyAdaValue :: Sdk.FullTxOut -> Bool
onlyAdaValue Sdk.FullTxOut{..} = 
  let
    value = Map.toList $ getValue fullTxOutValue
    isAdaCs = L.length  value == 1
    isAdaTn = case (L.headMaybe value) of
                  Just (_, tns) -> L.length (Map.toList tns) == 1
                  _ -> False
  in isAdaCs && isAdaTn


dummyAddr :: Sdk.ChangeAddress
dummyAddr =
  Sdk.ChangeAddress $ P.pubKeyHashAddress $ P.PubKeyHash $ P.BuiltinByteString (B8.pack $ show (0 :: Word64))
