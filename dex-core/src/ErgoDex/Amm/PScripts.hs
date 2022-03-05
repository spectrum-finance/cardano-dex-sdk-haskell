module ErgoDex.Amm.PScripts
  ( poolValidator
  , mkAllowedActions
  , mkSwapPolicy
  , mkDepositPolicy
  , mkRedeemPolicy
  , mkAllowedActions1
  , swapDatum1
  ) where

import Plutus.V1.Ledger.Api (MintingPolicy, Validator, Redeemer)

import qualified ErgoDex.PContracts.PPool as PP
import qualified ErgoDex.Contracts.Pool   as P

import Plutarch
import Plutarch.Prelude
import Plutarch.Builtin         (pasInt, pasByteStr, pasList)
import Plutarch.Api.V1          (mkMintingPolicy, mintingPolicySymbol, mkValidator)
import Plutarch.Api.V1.Value    (PCurrencySymbol(..))
import Plutarch.Api.V1.Contexts (PScriptContext)
import Plutus.V1.Ledger.Api (Datum(..), toBuiltinData, DatumHash(..), TxOutRef, Value, Redeemer (Redeemer))

mkMerkleMintingValidator
  :: Term s (PInteger :--> PScriptContext :--> PBool)
  -> Term s (PData :--> PScriptContext :--> POpaque)
mkMerkleMintingValidator validator =
  plam $ \dt ctx ->
    let
      datum     = pasInt # dt
      result    = validator # datum # ctx
    in popaque result

mkSwapPolicy :: P.PoolConfig -> MintingPolicy
mkSwapPolicy conf = mkMintingPolicy $ mkMerkleMintingValidator $ PP.mkSwapValidator (pconstant conf)

mkRedeemPolicy :: P.PoolConfig -> MintingPolicy
mkRedeemPolicy conf = mkMintingPolicy $ mkMerkleMintingValidator $ PP.mkRedeemValidator (pconstant conf)

mkDepositPolicy :: P.PoolConfig -> MintingPolicy
mkDepositPolicy conf = mkMintingPolicy $ mkMerkleMintingValidator $ PP.mkDepositValidator (pconstant conf)

mkAllowedActions1 :: P.PoolConfig -> Redeemer
mkAllowedActions1 conf =
  let
    swapSymbol    = mintingPolicySymbol $ mkSwapPolicy conf
    depositSymbol = mintingPolicySymbol $ mkDepositPolicy conf
    redeemSymbol  = mintingPolicySymbol $ mkRedeemPolicy conf
    list1 = [swapSymbol, depositSymbol, redeemSymbol]
  in Redeemer . toBuiltinData $ list1

swapDatum1 :: P.PoolConfig -> Datum
swapDatum1 conf =
  let
    swapSymbol    = mintingPolicySymbol $ mkSwapPolicy conf
  in Datum . toBuiltinData $ swapSymbol

mkAllowedActions :: P.PoolConfig -> Term s (PBuiltinList PCurrencySymbol)
mkAllowedActions conf =
    phoistAcyclic
      $ pcons # pconstant swapSymbol #$ pcons # pconstant depositSymbol #$ pcons # pconstant redeemSymbol # pnil
  where
    swapSymbol    = mintingPolicySymbol $ mkSwapPolicy conf
    depositSymbol = mintingPolicySymbol $ mkDepositPolicy conf
    redeemSymbol  = mintingPolicySymbol $ mkRedeemPolicy conf

poolValidatorT :: Term s (PData :--> PData :--> PScriptContext :--> POpaque)
poolValidatorT = plam $ \datum redeemer ctx -> unTermCont $ do
  let
    actionNft      = pcon $ PCurrencySymbol $ pasByteStr # redeemer
    allowedActions = pmap # plam (\d -> pcon $ PCurrencySymbol $ pasByteStr # d) # (pasList # datum)
    result         = PP.merklizedPoolValidator # allowedActions # actionNft # ctx
  pure $ popaque result

poolValidator :: Validator
poolValidator = mkValidator poolValidatorT
