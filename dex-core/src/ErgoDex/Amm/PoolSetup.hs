module ErgoDex.Amm.PoolSetup where

import qualified Data.Set                   as Set
import           RIO
import           Control.Monad.Trans.Either (hoistEither, runEitherT)
import           Control.Monad.Trans.Except (ExceptT(ExceptT))

import           Ledger          (Address, StakePubKeyHash, PaymentPubKeyHash, pubKeyHashAddress)
import qualified Ledger.Interval as Interval
import           Ledger.Value    (AssetClass)

import           ErgoDex.Types
import           ErgoDex.State
import           ErgoDex.Amm.Pool        (Pool(..), initPool)
import           ErgoDex.Amm.Constants
import qualified ErgoDex.Contracts.Typed as P
import           ErgoDex.Contracts.Types
import           CardanoTx.Models
import           ErgoDex.Contracts.Pool  (maxLqCapAmount)

data SetupExecError =
    MissingAsset AssetClass
  | MissingPool
  | InvalidNft
  | InvalidLiquidity
  | InsufficientInputs
  deriving Show

data PoolSetup m = PoolSetup
  { poolDeploy :: PaymentPubKeyHash -> Maybe StakePubKeyHash -> P.PoolConfig -> [FullTxOut] -> m (Either SetupExecError TxCandidate)
  }

burnLqInitial :: Amount Liquidity
burnLqInitial = Amount 1000 -- todo: aggregate protocol constants

mkPoolSetup :: (MonadIO m) => Address -> PoolSetup m
mkPoolSetup changeAddr = PoolSetup
  { poolDeploy = poolDeploy' burnLqInitial changeAddr
  }

poolDeploy'
  :: forall m. (MonadIO m)
  => Amount Liquidity 
  -> Address
  -> PaymentPubKeyHash
  -> Maybe StakePubKeyHash
  -> P.PoolConfig
  -> [FullTxOut]
  -> m (Either SetupExecError TxCandidate)
poolDeploy' burnLq changeAddr rewardPkh stakePkh pp@P.PoolConfig{..} utxosIn = runEitherT $ do
  inNft <- hoistEither $ overallAmountOf utxosIn poolNft
  inLq  <- hoistEither $ overallAmountOf utxosIn poolLq
  inX   <- hoistEither $ overallAmountOf utxosIn poolX
  inY   <- hoistEither $ overallAmountOf utxosIn poolY

  unless (amountEq inNft 1) (hoistEither $ Left InvalidNft) -- make sure valid NFT is provided
  unless (getAmount inLq == maxLqCapAmount) (hoistEither $ Left InvalidLiquidity) -- make sure valid amount of LQ tokens is provided

  (Predicted poolOutput nextPool, unlockedLq) <-
    ExceptT $ initPool pp burnLq (getAmount inX, getAmount inY) <&> (mapLeft (const InvalidLiquidity))

  let
    mintLqValue  = coinAmountValue (poolCoinLq nextPool) unlockedLq
    rewardOutput = TxOutCandidate
      { txOutCandidateAddress = pubKeyHashAddress rewardPkh stakePkh
      , txOutCandidateValue   = mintLqValue <> minSafeOutputValue
      , txOutCandidateDatum   = EmptyDatum
      }

    inputs  = utxosIn <&> mkPkhTxIn
    outputs = [poolOutput, rewardOutput]

    overallAdaOut = assetAmountOfCoin totalValueIn adaCoin
      where totalValueIn = foldr ( (<>) . txOutCandidateValue) mempty outputs

  overallAdaIn <- hoistEither $ overallAmountOf utxosIn adaCoin
  unless (overallAdaIn >= overallAdaOut) (hoistEither $ Left InsufficientInputs)

  hoistEither $ Right $ TxCandidate
    { txCandidateInputs       = Set.fromList inputs
    , txCandidateOutputs      = [poolOutput, rewardOutput]
    , txCandidateValueMint    = mempty -- todo: mint NFT and LQ right there?
    , txCandidateMintInputs   = mempty
    , txCandidateChangePolicy = Just $ ReturnTo changeAddr
    , txCandidateValidRange   = Interval.always
    , txCandidateSigners      = mempty
    }

overallAmountOf :: [FullTxOut] -> Coin a -> Either SetupExecError (AssetAmount a)
overallAmountOf utxos c =
    if amountEq coinAmount 0
    then Left $ MissingAsset $ unCoin c
    else Right coinAmount
  where
    totalValueIn = foldr ( (<>) . fullTxOutValue) mempty utxos
    coinAmount   = assetAmountOfCoin totalValueIn c
