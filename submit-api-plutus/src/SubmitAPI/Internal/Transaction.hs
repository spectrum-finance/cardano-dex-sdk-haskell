module SubmitAPI.Internal.Transaction where

import qualified Data.Set            as Set
import qualified Data.Map            as Map
import           Data.Functor

import qualified CardanoTx.Models            as Sdk
import qualified Ledger                      as P
import qualified Ledger.Constraints          as P
import qualified Ledger.Constraints.OffChain as P

import CardanoTx.ToPlutus (ToPlutus(toPlutus))

mkUnbalancedTx :: Set.Set Sdk.FullCollateralTxIn -> Sdk.TxCandidate -> P.UnbalancedTx
mkUnbalancedTx collateral tx@Sdk.TxCandidate{..} =
    P.UnbalancedTx
      { unBalancedTxTx                  = mkPlutusTx collateral tx
      , unBalancedTxRequiredSignatories = Map.fromList $ txCandidateSigners >>= (\pkh -> [(pkh, Nothing)])
      , unBalancedTxUtxoIndex           = Map.mapMaybe Sdk.toScriptOutput inputsIndex
      , unBalancedTxValidityTimeRange   = P.always -- todo
      }
  where
    inputsIndex =
      Map.fromList $ txCandidateInputs <&> (\i@Sdk.FullTxIn{fullTxInTxOut=Sdk.FullTxOut{..}} -> (fullTxOutRef, i))

mkPlutusTx :: Set.Set Sdk.FullCollateralTxIn -> Sdk.TxCandidate -> P.Tx
mkPlutusTx collateral Sdk.TxCandidate{..} =
  P.Tx
    { txInputs      = Set.fromList $ txCandidateInputs <&> toPlutus
    , txCollateral  = Set.fromList $ Set.elems collateral <&> toPlutus
    , txOutputs     = txCandidateOutputs <&> toPlutus
    , txMint        = Sdk.unMintValue txCandidateValueMint
    , txFee         = mempty
    , txValidRange  = txCandidateValidRange
    , txMintScripts = Sdk.mintInputsPolicies txCandidateMintInputs
    , txSignatures  = mempty
    , txRedeemers   = mempty -- todo
    , txData        = collectOutputsData txCandidateOutputs
    }

collectOutputsData :: [Sdk.TxOutCandidate] -> Map.Map P.DatumHash P.Datum
collectOutputsData outputs =
    Map.fromList $ outputs >>= tryGetDatum
  where
    tryGetDatum Sdk.TxOutCandidate{txOutCandidateDatum=Just d} = pure (P.datumHash d, d)
    tryGetDatum _                                              = mempty
