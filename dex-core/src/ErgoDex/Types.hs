module ErgoDex.Types where

import Prelude             (Show, Eq, Ord(..), Integer, Bool, ($), (==), (<>), negate)
import Playground.Contract (FromJSON, ToJSON, Generic)

import           Ledger
import           Ledger.Value      (AssetClass(..), assetClassValueOf, assetClassValue, Value(..))
import           PlutusTx.Numeric  (AdditiveSemigroup(..), MultiplicativeSemigroup(..))
import qualified PlutusTx.AssocMap as Map

import ErgoDex.Contracts.Types as Currencies

data Lovelace = Lovelace
  deriving (Show, Eq)

newtype AssetEntry = AssetEntry { unAssetEntry :: (AssetClass, Integer) }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

assetEntryClass :: AssetEntry -> AssetClass
assetEntryClass (AssetEntry (cls, _)) = cls

assetEntry :: CurrencySymbol -> TokenName -> Integer -> AssetEntry
assetEntry cs tn v = AssetEntry (AssetClass (cs, tn), v)

data AssetAmount a = AssetAmount
  { getAsset  :: Coin a
  , getAmount :: Amount a
  } deriving (Show, Eq)

instance AdditiveSemigroup (AssetAmount a) where
  a0 + a1 = a0 { getAmount = (getAmount a0) + (getAmount a1) }

instance MultiplicativeSemigroup (AssetAmount a) where
  a0 * a1 = a0 { getAmount = (getAmount a0) * (getAmount a1) }

instance Ord (AssetAmount a) where
  compare (AssetAmount _ (Amount x)) (AssetAmount _ (Amount y)) = compare x y

retagCoin :: forall a b . Coin a -> Coin b
retagCoin (Coin ac) = Coin ac

amountEq :: AssetAmount a -> Integer -> Bool
amountEq (AssetAmount _ (Amount a)) b = a == b

assetAmountSubtract :: Value -> AssetAmount a -> Value
assetAmountSubtract vl AssetAmount{getAsset=Coin ac, getAmount=Amount v} =
    vl <> negValue
  where
    (cs, tn) = unAssetClass ac
    negValue = Value $ Map.fromList [(cs, Map.fromList [(tn, negate v)])]

assetAmountRawValue :: AssetAmount a -> Integer
assetAmountRawValue AssetAmount{getAmount=Amount v} = v

assetAmountValue :: AssetAmount a -> Value
assetAmountValue AssetAmount{getAsset=Coin ac, getAmount=Amount v} = assetClassValue ac v

assetAmountOf :: AssetEntry -> AssetAmount a
assetAmountOf (AssetEntry (ac, v)) = AssetAmount (Coin ac) (Amount v)

assetAmountCoinOf :: Coin a -> Integer -> AssetAmount a
assetAmountCoinOf c v = AssetAmount c (Amount v)

assetAmountPairOf :: (AssetEntry, AssetEntry) -> Coin a -> AssetAmount a
assetAmountPairOf (AssetEntry (ac, av), AssetEntry (bc, bv)) c =
  AssetAmount c (Amount $
    if ac == (unCoin c) then av
    else if bc == (unCoin c) then bv
    else 0)

assetAmountOfCoin :: Value -> Coin a -> AssetAmount a
assetAmountOfCoin v c =
  AssetAmount c (Amount $ assetClassValueOf v (unCoin c))

data ExFeePerToken = ExFeePerToken
  { exFeePerTokenNum :: Integer
  , exFeePerTokenDen :: Integer
  } deriving (Show, Eq, Generic, ToJSON, FromJSON)

newtype ExFee = ExFee { unExFee :: Amount Currencies.Lovelace }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)
