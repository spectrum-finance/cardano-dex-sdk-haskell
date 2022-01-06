module ErgoDex.Amm.PoolSetup where

import           Control.Monad           (unless)
import           Data.Foldable
import           Data.Either.Combinators (maybeToRight, mapLeft)
import qualified Data.Set                as Set

import           Ledger                          (PubKeyHash(..), Datum(..), Address, pubKeyHashAddress)
import qualified Ledger.Interval as Interval
import           Ledger.Value                    (AssetClass)
import qualified Ledger.Typed.Scripts.Validators as Validators
import           PlutusTx                        (toBuiltinData)

import           ErgoDex.Types
import           ErgoDex.State
import           ErgoDex.Class
import           ErgoDex.Amm.Pool       (Pool(..), applyInit)
import qualified ErgoDex.Contracts.Pool as P
import           ErgoDex.Contracts.Types
import           ErgoDex.OffChain
import           CardanoTx.Models

data SetupExecError =
    MissingAsset AssetClass
  | MissingPool
  | InvalidNft
  | InvalidLiquidity
  deriving (Show)

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
  unless (amountEq inNft 1) (Left InvalidNft) -- make sure valid NFT is provided
  let
    poolOutput = TxOutCandidate
      { txOutCandidateAddress = Validators.validatorAddress poolInstance
      , txOutCandidateValue   = assetAmountValue inNft
      , txOutCandidateDatum   = Just $ Datum $ PlutusTx.toBuiltinData pp
      }

  Right $ TxCandidate
    { txCandidateInputs       = Set.fromList inputs
    , txCandidateOutputs      = [poolOutput]
    , txCandidateValueMint    = mempty -- todo: mint NFT and LQ right there?
    , txCandidateMintInputs   = mempty
    , txCandidateChangePolicy = Just $ ReturnTo changeAddr
    , txCandidateValidRange   = Interval.always
    }

poolInit' :: Address -> [FullTxIn] -> PubKeyHash -> Either SetupExecError TxCandidate
poolInit' changeAddr inputs rewardPkh = do
  let
    poolAddress    = Validators.validatorAddress poolInstance
    maybePoolInput = find (\i -> (fullTxOutAddress . fullTxInTxOut) i == poolAddress) inputs

  poolInput        <- maybeToRight MissingPool maybePoolInput
  Confirmed _ pool <- maybeToRight MissingPool (parseFromLedger $ fullTxInTxOut poolInput)

  inX <- tryGetInputAmountOf inputs (poolCoinX pool)
  inY <- tryGetInputAmountOf inputs (poolCoinY pool)

  Predicted poolOutput nextPool <- mapLeft (const InvalidLiquidity) (applyInit pool (getAmount inX, getAmount inY))

  let
    inputsReordered = poolInput : filter (/= poolInput) inputs

    mintLqValue = coinAmountValue (poolCoinLq nextPool) (poolLiquidity nextPool)

    outputs = [poolOutput, rewardOutput]
      where
        rewardOutput = TxOutCandidate
          { txOutCandidateAddress = pubKeyHashAddress rewardPkh
          , txOutCandidateValue   = mintLqValue
          , txOutCandidateDatum   = Nothing
          }

  Right $ TxCandidate
    { txCandidateInputs       = Set.fromList inputsReordered
    , txCandidateOutputs      = outputs
    , txCandidateValueMint    = mempty
    , txCandidateMintInputs   = mempty
    , txCandidateChangePolicy = Just $ ReturnTo changeAddr
    , txCandidateValidRange   = Interval.always
    }

tryGetInputAmountOf :: [FullTxIn] -> Coin a -> Either SetupExecError (AssetAmount a)
tryGetInputAmountOf inputs c =
    if amountEq coinAmount 0
    then Right coinAmount
    else Left $ MissingAsset $ unCoin c
  where
    totalValueIn = foldr ( (<>) . fullTxOutValue . fullTxInTxOut) mempty inputs
    coinAmount   = assetAmountOfCoin totalValueIn c
