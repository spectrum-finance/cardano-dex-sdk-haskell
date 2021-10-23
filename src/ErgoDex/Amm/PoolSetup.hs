module ErgoDex.Amm.PoolSetup where

import Control.Monad           (when)
import Data.Foldable
import Data.Either.Combinators (maybeToRight, mapLeft)

import           Ledger                          (PubKeyHash(..), Datum(..), Address, pubKeyHashAddress)
import           Ledger.Value                    (AssetClass)
import qualified Ledger.Typed.Scripts.Validators as Validators
import           PlutusTx                        (toBuiltinData)

import           ErgoDex.Types
import           ErgoDex.State
import           ErgoDex.Class
import           ErgoDex.Amm.Pool       (Pool(..), PoolId(..), applyInit)
import qualified ErgoDex.Contracts.Pool as P
import           ErgoDex.Contracts.Types
import           ErgoDex.OffChain
import           Cardano.Models
import           Cardano.Utils

data SetupExecError =
    MissingAsset AssetClass
  | MissingPool
  | InvalidNft
  | InvalidLiquidity

data PoolSetup = PoolSetup
  { poolDeploy :: P.PoolParams -> [FullTxIn] -> Either SetupExecError TxCandidate
  , poolInit   :: [FullTxIn]   -> PubKeyHash -> Either SetupExecError TxCandidate
  }

mkPoolSetup :: Address -> PoolSetup
mkPoolSetup changeAddr = PoolSetup
  { poolDeploy = poolDeploy' changeAddr
  , poolInit   = poolInit' changeAddr
  }

poolDeploy' :: Address -> P.PoolParams -> [FullTxIn] -> Either SetupExecError TxCandidate
poolDeploy' changeAddr pp@P.PoolParams{..} inputs = do
  inNft <- tryGetInputAmountOf inputs poolNft
  when (not (amountEq inNft 1)) (Left InvalidNft) -- make sure valid NFT is provided
  let
    poolOutput = TxOutCandidate
      { txOutCandidateAddress  = Validators.validatorAddress poolInstance
      , txOutCandidateValue    = assetAmountValue inNft
      , txOutCandidateDatum    = Just $ Datum $ PlutusTx.toBuiltinData pp
      , txOutCandidatePolicies = []
      }

    outputs = [poolOutput]

  Right $ TxCandidate inputs outputs (Just $ ReturnTo changeAddr)

poolInit' :: Address -> [FullTxIn] -> PubKeyHash -> Either SetupExecError TxCandidate
poolInit' changeAddr inputs rewardPkh = do
  let
    poolAddress    = Validators.validatorAddress poolInstance
    maybePoolInput = find (\i -> (fullTxOutAddress . fullTxInTxOut) i == poolAddress) inputs

  poolInput        <- maybeToRight MissingPool maybePoolInput
  Confirmed _ pool <- maybeToRight MissingPool (parseFromLedger $ fullTxInTxOut poolInput)

  inX <- tryGetInputAmountOf inputs (poolCoinX pool)
  inY <- tryGetInputAmountOf inputs (poolCoinY pool)

  Predicted poolOutput nextPool <- mapLeft (\_ -> InvalidLiquidity) (applyInit pool (getAmount inX, getAmount inY))

  let
    inputsReordered = [poolInput] ++ (filter (\i -> i /= poolInput) inputs)
    outputs =
        [poolOutput, rewardOutput]
      where
        rewardValue  = coinAmountValue (poolCoinLq nextPool) (poolLiquidity nextPool)
        rewardOutput = TxOutCandidate
          { txOutCandidateAddress  = pubKeyHashAddress rewardPkh
          , txOutCandidateValue    = rewardValue
          , txOutCandidateDatum    = Nothing
          , txOutCandidatePolicies = [liquidityMintingPolicyInstance (unPoolId $ poolId nextPool)]
          }

  Right $ TxCandidate inputsReordered outputs (Just $ ReturnTo changeAddr)

tryGetInputAmountOf :: [FullTxIn] -> Coin a -> Either SetupExecError (AssetAmount a)
tryGetInputAmountOf inputs c =
    if (amountEq coinAmount 0)
    then Right coinAmount
    else Left $ MissingAsset $ unCoin c
  where
    totalValueIn = foldr ( (<>) . fullTxOutValue . fullTxInTxOut) mempty inputs
    coinAmount   = assetAmountOfCoin totalValueIn c
