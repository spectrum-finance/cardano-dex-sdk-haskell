module WalletAPI.UtxoStore where

import           RIO
import qualified Data.Set               as Set
import qualified Data.Map               as Map

import Ledger
import CardanoTx.Models

type Store = Map.Map TxOutRef FullTxOut

data UtxoStore f = UtxoStore
  { put      :: Set.Set FullTxOut -> f ()
  , get      :: f (Set.Set FullTxOut)
  , drop     :: Set.Set TxOutRef -> f ()
  , contains :: TxOutRef -> f Bool
  }

mkUtxoStore :: MonadIO f => f (UtxoStore f)
mkUtxoStore = do
  storeT <- liftIO $ newTVarIO mempty
  pure $ UtxoStore
    { put      = put' storeT
    , get      = get' storeT
    , drop     = drop' storeT
    , contains = contains' storeT
    }

put' :: MonadIO f => TVar Store -> Set.Set FullTxOut -> f ()
put' storeT outs =
  liftIO $ atomically $ do
    store <- readTVar storeT
    let store' = Map.union store (Map.fromList $ Set.elems outs <&> (\o -> (fullTxOutRef o, o)))
    writeTVar storeT store'

get' :: MonadIO f => TVar Store -> f (Set.Set FullTxOut)
get' storeT = liftIO $ atomically $ readTVar storeT <&> Set.fromList . Map.elems

drop' :: MonadIO f => TVar Store -> Set.Set TxOutRef -> f ()
drop' storeT orefs =
  liftIO $ atomically $ do
    store <- readTVar storeT
    let store' = foldr Map.delete store orefs
    writeTVar storeT store'

contains' :: MonadIO f => TVar Store -> TxOutRef -> f Bool
contains' storeT ref = liftIO $ atomically $ readTVar storeT <&> Map.member ref
