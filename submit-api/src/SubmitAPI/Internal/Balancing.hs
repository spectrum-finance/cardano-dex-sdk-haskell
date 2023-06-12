module SubmitAPI.Internal.Balancing where

import Prelude

import           RIO.List       (find)
import           RIO            (isJust)
import           Data.Bifunctor (first)
import           Data.Map       (Map)
import qualified Data.Map       as Map
import           Data.Maybe     (fromMaybe, catMaybes)
import           Data.Functor   ((<&>))
import           Data.Set       (Set)
import           Data.Ratio

import Cardano.Api
import Cardano.Api.Shelley   (ProtocolParameters(..), PoolId, ReferenceScript (..), fromShelleyLovelace)
import qualified Cardano.Ledger.Coin as Ledger

makeTransactionBodyAutoBalance
  :: forall era mode.
     IsShelleyBasedEra era
  => EraInMode era mode
  -> SystemStart
  -> EraHistory mode
  -> ProtocolParameters
  -> Set PoolId       -- ^ The set of registered stake pools
  -> UTxO era         -- ^ Just the transaction inputs, not the entire 'UTxO'.
  -> TxBodyContent BuildTx era
  -> AddressInEra era -- ^ Change address
  -> Maybe Word       -- ^ Override key witnesses
  -> Either TxBodyErrorAutoBalance (BalancedTxBody era)
makeTransactionBodyAutoBalance eraInMode systemstart history pparams
                            poolids utxo txbodycontent changeaddr mnkeys = do

    -- Our strategy is to:
    -- 1. evaluate all the scripts to get the exec units, update with ex units
    -- 2. figure out the overall min fees
    -- 3. update tx with fees
    -- 4. balance the transaction and update tx change output

    txbody0 <-
      first TxBodyError $ makeTransactionBody txbodycontent
        { txOuts =
            txOuts txbodycontent ++ [TxOut changeaddr (lovelaceToTxOutValue 0) TxOutDatumNone ReferenceScriptNone]
            --TODO: think about the size of the change output
            -- 1,2,4 or 8 bytes?
        }

    exUnitsMap <- first TxBodyErrorValidityInterval $
                    evaluateTransactionExecutionUnits
                      eraInMode
                      systemstart
                      history
                      pparams
                      utxo
                      txbody0

    exUnitsMap' <-
      case Map.mapEither id exUnitsMap of
        (failures, exUnitsMap') ->
          handleExUnitsErrors
            (txScriptValidityToScriptValidity (txScriptValidity txbodycontent))
            failures
            exUnitsMap'

    txbodycontent1 <- substituteExecutionUnits exUnitsMap' txbodycontent

    explicitTxFees <- first (const TxBodyErrorByronEraNotSupported) $
                        txFeesExplicitInEra era'

    -- Make a txbody that we will use for calculating the fees. For the purpose
    -- of fees we just need to make a txbody of the right size in bytes. We do
    -- not need the right values for the fee or change output. We use
    -- "big enough" values for the change output and set so that the CBOR
    -- encoding size of the tx will be big enough to cover the size of the final
    -- output and fee. Yes this means this current code will only work for
    -- final fee of less than around 4000 ada (2^32-1 lovelace) and change output
    -- of less than around 18 trillion ada  (2^64-1 lovelace).
    let (dummyCollRet, dummyTotColl) = maybeDummyTotalCollAndCollReturnOutput txbodycontent changeaddr
    txbody1 <- first TxBodyError $ -- TODO: impossible to fail now
               makeTransactionBody txbodycontent1 {
                 txFee  = TxFeeExplicit explicitTxFees $ Lovelace (2^(32 :: Integer) - 1),
                 txOuts = txOuts txbodycontent ++
                            [TxOut changeaddr
                              (lovelaceToTxOutValue $ Lovelace (2^(64 :: Integer)) - 1)
                              TxOutDatumNone ReferenceScriptNone
                            ],
                 txReturnCollateral = dummyCollRet,
                 txTotalCollateral = dummyTotColl
               }

    let nkeys = fromMaybe (estimateTransactionKeyWitnessCount txbodycontent1)
                          mnkeys
        fee   = evaluateTransactionFee pparams txbody1 nkeys 0 --TODO: byron keys
        (retColl, reqCol) = calcReturnAndTotalCollateral
                              fee pparams (txInsCollateral txbodycontent)
                              (txReturnCollateral txbodycontent)
                              (txTotalCollateral txbodycontent) changeaddr utxo

    -- Make a txbody for calculating the balance. For this the size of the tx
    -- does not matter, instead it's just the values of the fee and outputs.
    -- Here we do not want to start with any change output, since that's what
    -- we need to calculate.

    txbody2 <- first TxBodyError $ -- TODO: impossible to fail now
               makeTransactionBody txbodycontent1 {
                 txFee = TxFeeExplicit explicitTxFees fee
               }

    let balance = evaluateTransactionBalance pparams poolids utxo txbody2

    mapM_ (`checkMinUTxOValue` pparams) $ txOuts txbodycontent1

    -- check if the balance is positive or negative
    -- in one case we can produce change, in the other the inputs are insufficient
    case balance of
      TxOutAdaOnly _ _ -> balanceCheck balance (txOuts txbodycontent1)
      TxOutValue _ v   ->
        case valueToLovelace v of
          Nothing -> Left $ TxBodyErrorNonAdaAssetsUnbalanced v
          Just _  -> balanceCheck balance (txOuts txbodycontent1)

    --TODO: we could add the extra fee for the CBOR encoding of the change,
    -- now that we know the magnitude of the change: i.e. 1-8 bytes extra.

    -- The txbody with the final fee and change output. This should work
    -- provided that the fee and change are less than 2^32-1, and so will
    -- fit within the encoding size we picked above when calculating the fee.
    -- Yes this could be an over-estimate by a few bytes if the fee or change
    -- would fit within 2^16-1. That's a possible optimisation.
    txbody3 <-
      first TxBodyError $ -- TODO: impossible to fail now
        makeTransactionBody txbodycontent1 {
          txFee  = TxFeeExplicit explicitTxFees fee,
          txOuts = accountForNoChange
                     (TxOut changeaddr balance TxOutDatumNone ReferenceScriptNone)
                     (txOuts txbodycontent),
          txReturnCollateral = retColl,
          txTotalCollateral = reqCol
        }
    return (BalancedTxBody txbody3 (TxOut changeaddr balance TxOutDatumNone ReferenceScriptNone) fee)
 where
   era :: ShelleyBasedEra era
   era = shelleyBasedEra

   era' :: CardanoEra era
   era' = cardanoEra

   calcReturnAndTotalCollateral
     :: Lovelace -- ^ Fee
     -> ProtocolParameters
     -> TxInsCollateral era -- ^ From the initial TxBodyContent
     -> TxReturnCollateral CtxTx era -- ^ From the initial TxBodyContent
     -> TxTotalCollateral era -- ^ From the initial TxBodyContent
     -> AddressInEra era -- ^ Change address
     -> UTxO era
     -> (TxReturnCollateral CtxTx era, TxTotalCollateral era)
   calcReturnAndTotalCollateral _ _ TxInsCollateralNone _ _ _ _= (TxReturnCollateralNone, TxTotalCollateralNone)
   calcReturnAndTotalCollateral _ _ _ rc@TxReturnCollateral{} tc@TxTotalCollateral{} _ _ = (rc,tc)
   calcReturnAndTotalCollateral fee pp (TxInsCollateral _ collIns) txReturnCollateral txTotalCollateral cAddr (UTxO utxo') = do

    case totalAndReturnCollateralSupportedInEra era' of
      Nothing -> (TxReturnCollateralNone, TxTotalCollateralNone)
      Just retColSup ->
        case protocolParamCollateralPercent pp of
          Nothing -> (TxReturnCollateralNone, TxTotalCollateralNone)
          Just colPerc -> do
            -- We must first figure out how much lovelace we have committed
            -- as collateral and we must determine if we have enough lovelace at our
            -- collateral tx inputs to cover the tx
            let txOuts = catMaybes [ Map.lookup txin utxo' | txin <- collIns]
                totalCollateralLovelace = mconcat $ map (\(TxOut _ txOutVal _ _) -> txOutValueToLovelace txOutVal) txOuts
                requiredCollateral@(Lovelace reqAmt) = fromIntegral colPerc * fee
                totalCollateral = TxTotalCollateral retColSup . fromShelleyLovelace
                                                              . Ledger.rationalToCoinViaCeiling
                                                              $ reqAmt % 100
                -- Why * 100? requiredCollateral is the product of the collateral percentage and the tx fee
                -- We choose to multiply 100 rather than divide by 100 to make the calculation
                -- easier to manage. At the end of the calculation we then use % 100 to perform our division
                -- and round up.
                enoughCollateral = totalCollateralLovelace * 100 >= requiredCollateral
                Lovelace amt = totalCollateralLovelace * 100 - requiredCollateral
                returnCollateral = fromShelleyLovelace . Ledger.rationalToCoinViaFloor $ amt % 100

            case (txReturnCollateral, txTotalCollateral) of
              (rc@TxReturnCollateral{}, tc@TxTotalCollateral{}) ->
                (rc, tc)
              (rc@TxReturnCollateral{}, TxTotalCollateralNone) ->
                (rc, TxTotalCollateralNone)
              (TxReturnCollateralNone, tc@TxTotalCollateral{}) ->
                (TxReturnCollateralNone, tc)
              (TxReturnCollateralNone, TxTotalCollateralNone) ->
                if enoughCollateral
                then
                  ( TxReturnCollateral
                      retColSup
                      (TxOut cAddr (lovelaceToTxOutValue returnCollateral) TxOutDatumNone ReferenceScriptNone)
                  , totalCollateral
                  )
                else (TxReturnCollateralNone, TxTotalCollateralNone)

   -- In the event of spending the exact amount of lovelace in
   -- the specified input(s), this function excludes the change
   -- output. Note that this does not save any fees because by default
   -- the fee calculation includes a change address for simplicity and
   -- we make no attempt to recalculate the tx fee without a change address.
   accountForNoChange :: TxOut CtxTx era -> [TxOut CtxTx era] -> [TxOut CtxTx era]
   accountForNoChange change@(TxOut addr balance _ _) rest =
     case txOutValueToLovelace balance of
       Lovelace 0 -> rest
       -- We append change at the end so a client can predict the indexes
       -- of the outputs
       chargeLovelace ->
        let
          chargeUserBox = find (\(TxOut boxAddr _ _ _) -> boxAddr == addr) rest
          updatedChargeUserBox = chargeUserBox <&> (\(TxOut boxAddr prevValue d ref) ->
               let
                 newValue =
                  case prevValue of
                    TxOutAdaOnly supportedEra lovelace -> TxOutAdaOnly supportedEra (lovelace + chargeLovelace)
                    TxOutValue multiSupport value -> TxOutValue multiSupport (value <> lovelaceToValue chargeLovelace)
                in TxOut boxAddr newValue d ref
            )
          outputs =
            case updatedChargeUserBox of
                Nothing -> rest ++ [change]
                Just output ->
                  -- replace user reward box with updated
                  (\txOut@(TxOut boxAddr _ _ _) -> if boxAddr == addr then output else txOut) <$> rest
        in outputs

   maybeDummyTotalCollAndCollReturnOutput
     :: TxBodyContent BuildTx era -> AddressInEra era -> (TxReturnCollateral CtxTx era, TxTotalCollateral era)
   maybeDummyTotalCollAndCollReturnOutput TxBodyContent{txInsCollateral, txReturnCollateral, txTotalCollateral} cAddr =
     case txInsCollateral of
       TxInsCollateralNone -> (TxReturnCollateralNone, TxTotalCollateralNone)
       TxInsCollateral{} ->
         case totalAndReturnCollateralSupportedInEra era' of
           Nothing -> (TxReturnCollateralNone, TxTotalCollateralNone)
           Just retColSup ->
             let dummyRetCol = TxReturnCollateral
                                 retColSup
                                 (TxOut cAddr (lovelaceToTxOutValue $ Lovelace (2^(64 :: Integer)) - 1)
                                 TxOutDatumNone ReferenceScriptNone)
                 dummyTotCol = TxTotalCollateral retColSup (Lovelace (2^(32 :: Integer) - 1))
             in case (txReturnCollateral, txTotalCollateral) of
                  (rc@TxReturnCollateral{}, tc@TxTotalCollateral{}) -> (rc, tc)
                  (rc@TxReturnCollateral{},TxTotalCollateralNone) -> (rc, dummyTotCol)
                  (TxReturnCollateralNone,tc@TxTotalCollateral{}) -> (dummyRetCol, tc)
                  (TxReturnCollateralNone, TxTotalCollateralNone) -> (dummyRetCol, dummyTotCol)

   balanceCheck :: TxOutValue era -> [TxOut CtxTx era] -> Either TxBodyErrorAutoBalance ()
   balanceCheck balance outs
    | txOutValueToLovelace balance == 0 = return ()
    | txOutValueToLovelace balance < 0 =
        Left . TxBodyErrorAdaBalanceNegative $ txOutValueToLovelace balance
    | otherwise = do
        let chargeBoxWillBeMerged = isJust $ find (\(TxOut boxAddr _ _ _) -> boxAddr == changeaddr) outs
        if chargeBoxWillBeMerged
          then 
            Right ()
          else 
            case checkMinUTxOValue (TxOut changeaddr balance TxOutDatumNone ReferenceScriptNone) pparams of
              Left (TxBodyErrorMinUTxONotMet txOutAny minUTxO) ->
                Left $ TxBodyErrorAdaBalanceTooSmall txOutAny minUTxO (txOutValueToLovelace balance)
              Left err -> Left err
              Right _  -> Right ()

   checkMinUTxOValue
     :: TxOut CtxTx era
     -> ProtocolParameters
     -> Either TxBodyErrorAutoBalance ()
   checkMinUTxOValue txout@(TxOut addr v _ _) pparams' = do
     minUTxO  <- first TxBodyErrorMinUTxOMissingPParams
                   $ calculateMinimumUTxO era txout pparams'
     let chargeBoxWillBeMerged = addr == changeaddr
     if txOutValueToLovelace v >= selectLovelace minUTxO || chargeBoxWillBeMerged
     then Right ()
     else Left TxBodyErrorMissingParamMinUTxO --todo fix: TxOutInAnyEra. Current err is incorrect

substituteExecutionUnits :: Map ScriptWitnessIndex ExecutionUnits
                         -> TxBodyContent BuildTx era
                         -> Either TxBodyErrorAutoBalance (TxBodyContent BuildTx era)
substituteExecutionUnits exUnitsMap =
    mapTxScriptWitnesses f
  where
    f :: ScriptWitnessIndex
      -> ScriptWitness witctx era
      -> Either TxBodyErrorAutoBalance (ScriptWitness witctx era)
    f _   wit@SimpleScriptWitness{} = Right wit
    f idx (PlutusScriptWitness langInEra version script datum redeemer _) =
      case Map.lookup idx exUnitsMap of
        Nothing ->
          Left $ TxBodyErrorScriptWitnessIndexMissingFromExecUnitsMap idx exUnitsMap
        Just exunits -> Right $ PlutusScriptWitness langInEra version script
                                            datum redeemer exunits

-- lovelaceToTxOutValue :: IsCardanoEra era => Lovelace -> TxOutValue era
-- lovelaceToTxOutValue l =
--     case multiAssetSupportedInEra cardanoEra of
--       Left adaOnly     -> TxOutAdaOnly adaOnly  l
--       Right multiAsset -> TxOutValue multiAsset (lovelaceToValue l)

handleExUnitsErrors ::
     ScriptValidity -- ^ Mark script as expected to pass or fail validation
  -> Map ScriptWitnessIndex ScriptExecutionError
  -> Map ScriptWitnessIndex ExecutionUnits
  -> Either TxBodyErrorAutoBalance (Map ScriptWitnessIndex ExecutionUnits)
handleExUnitsErrors ScriptValid failuresMap exUnitsMap =
    if null failures
      then Right exUnitsMap
      else Left (TxBodyScriptExecutionError failures)
  where failures :: [(ScriptWitnessIndex, ScriptExecutionError)]
        failures = Map.toList failuresMap
handleExUnitsErrors ScriptInvalid failuresMap exUnitsMap
  | null scriptFailures = Left TxBodyScriptBadScriptValidity
  | null nonScriptFailures = Right exUnitsMap
  | otherwise = Left (TxBodyScriptExecutionError nonScriptFailures)
  where nonScriptFailures :: [(ScriptWitnessIndex, ScriptExecutionError)]
        nonScriptFailures = filter (not . isScriptErrorEvaluationFailed) (Map.toList failuresMap)
        scriptFailures :: [(ScriptWitnessIndex, ScriptExecutionError)]
        scriptFailures = filter isScriptErrorEvaluationFailed (Map.toList failuresMap)
        isScriptErrorEvaluationFailed :: (ScriptWitnessIndex, ScriptExecutionError) -> Bool
        isScriptErrorEvaluationFailed (_, e) = case e of
            ScriptErrorEvaluationFailed _ _ -> True
            _                               -> True

txScriptValidityToScriptValidity :: TxScriptValidity era -> ScriptValidity
txScriptValidityToScriptValidity TxScriptValidityNone                = ScriptValid
txScriptValidityToScriptValidity (TxScriptValidity _ scriptValidity) = scriptValidity