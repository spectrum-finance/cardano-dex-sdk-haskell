

module ErgoDex.Amm.PScripts where

import           Plutus.V1.Ledger.Api
import qualified ErgoDex.PContracts.PPool as PP
import qualified ErgoDex.PContracts.PDeposit as PD
import Plutarch
import Plutarch.Prelude
import Plutarch.Builtin (pasInt, pasByteStr)
import PExtra.API
import Plutarch.DataRepr
import Plutarch.Api.V1 (mkMintingPolicy, PMintingPolicy(..), mintingPolicySymbol, mkValidator)
import Plutarch.Api.V1.Value (PCurrencySymbol(..), PValue(..))
import Plutarch.Lift
import Plutarch.Api.V1.Contexts

swapMintingValidator :: Term s (PData :--> PScriptContext :--> POpaque)
swapMintingValidator = plam $ \d ctx -> unTermCont $ do
  let
    intData = pasInt # d
    bool = swapValidator # pPPoolConfig # intData
  pure $ popaque bool

swapPolicy :: MintingPolicy
swapPolicy = mkMintingPolicy swapMintingValidator

swapCurSymbol :: CurrencySymbol
swapCurSymbol = mintingPolicySymbol swapPolicy

redeemMintingValidator :: Term s (PData :--> PScriptContext :--> POpaque)
redeemMintingValidator = plam $ \d ctx -> unTermCont $ do
  let
    intData = pasInt # d
    bool = redeemValidator # pPPoolConfig # intData
  pure $ popaque bool

redeemPolicy :: MintingPolicy
redeemPolicy = mkMintingPolicy redeemMintingValidator

redeemCurSymbol :: CurrencySymbol
redeemCurSymbol = mintingPolicySymbol redeemPolicy

depositMintingValidator :: Term s (PData :--> PScriptContext :--> POpaque)
depositMintingValidator = plam $ \d ctx -> unTermCont $ do
  let
    intData = pasInt # d
    bool = depositValidator # pPPoolConfig # intData
  pure $ popaque bool

depositPolicy :: MintingPolicy
depositPolicy = mkMintingPolicy depositMintingValidator

depositCurSymbol :: CurrencySymbol
depositCurSymbol = mintingPolicySymbol depositPolicy

allowedActions :: Term s (PBuiltinList PCurrencySymbol)
allowedActions = phoistAcyclic $
  pcons # pconstant depositCurSymbol #$ pcons # pconstant redeemCurSymbol #$ pcons # pconstant swapCurSymbol # pnil

poolValidatorT :: Term s (PData :--> PData :--> PScriptContext :--> POpaque)
poolValidatorT = plam $ \_ red ctx -> unTermCont $ do
  let
    actionNft = pcon $ PCurrencySymbol $ pasByteStr # red
    validator = mkMerklizedPoolValidator allowedActions
    bool      = validator # pcon PUnit # actionNft # ctx
  pure $ popaque bool

poolValidator = unsafeMkTypedValidator $ mkValidator poolValidatorT

depositValidator = unsafeMkTypedValidator PD.validator
