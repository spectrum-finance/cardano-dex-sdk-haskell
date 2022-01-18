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
import           ErgoDex.Amm.Constants
import qualified ErgoDex.Contracts.Pool as P
import           ErgoDex.Contracts.Types
import           ErgoDex.Contracts.OffChain
import           CardanoTx.Models
import           ErgoDex.Contracts.Pool (maxLqCap)

data SetupExecError =
    MissingAsset AssetClass
  | MissingPool
  | InvalidNft
  | InvalidLiquidity
  | InsufficientInputs
  deriving Show

data PoolSetup = PoolSetup
  { poolDeploy :: P.PoolDatum -> [FullTxIn] -> Either SetupExecError TxCandidate
  , poolInit   :: [FullTxIn]   -> PubKeyHash -> Either SetupExecError TxCandidate
  }

mkPoolSetup :: Address -> PoolSetup
mkPoolSetup changeAddr = PoolSetup
  { poolDeploy = poolDeploy' changeAddr
  , poolInit   = poolInit' changeAddr
  }

poolDeploy' :: Address -> P.PoolDatum -> [FullTxIn] -> Either SetupExecError TxCandidate
poolDeploy' changeAddr pp@P.PoolDatum{..} inputs = do
  inNft <- tryGetInputAmountOf inputs poolNft
  inLq  <- tryGetInputAmountOf inputs poolLq
  unless (amountEq inNft 1) (Left InvalidNft) -- make sure valid NFT is provided
  unless (getAmount inLq == maxLqCap) (Left InvalidLiquidity) -- make sure valid amount of LQ tokens is provided
  let
    poolOutput = TxOutCandidate
      { txOutCandidateAddress = Validators.validatorAddress poolInstance
      , txOutCandidateValue   = assetAmountValue inNft <> assetAmountValue inLq <> minSafeOutputValue
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
    maybePoolInput = find ((== poolAddress) . fullTxOutAddress . fullTxInTxOut) inputs

  poolInput        <- maybeToRight MissingPool maybePoolInput
  Confirmed _ pool <- maybeToRight MissingPool (parseFromLedger $ fullTxInTxOut poolInput)

  inX <- tryGetInputAmountOf inputs (poolCoinX pool)
  inY <- tryGetInputAmountOf inputs (poolCoinY pool)

  (Predicted poolOutput nextPool, unlockedLq) <-
    mapLeft (const InvalidLiquidity) (applyInit pool (getAmount inX, getAmount inY))

  let
    inputsReordered = poolInput : filter (/= poolInput) inputs

    mintLqValue = coinAmountValue (poolCoinLq nextPool) unlockedLq

    outputs = [poolOutput, rewardOutput]
      where
        rewardOutput = TxOutCandidate
          { txOutCandidateAddress = pubKeyHashAddress rewardPkh
          , txOutCandidateValue   = mintLqValue <> minSafeOutputValue
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
    then Left $ MissingAsset $ unCoin c
    else Right coinAmount
  where
    totalValueIn = foldr ( (<>) . fullTxOutValue . fullTxInTxOut) mempty inputs
    coinAmount   = assetAmountOfCoin totalValueIn c
