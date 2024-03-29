{-# LANGUAGE LambdaCase #-}
module WalletAPI.UtxoStore where

import           RIO
import qualified Data.Set               as Set
import qualified Data.Map               as Map
import qualified Database.RocksDB as Rocks
import Control.Monad.Trans.Resource
  ( MonadResource )

import Ledger
import System.Logging.Hlog
import CardanoTx.Models ( FullTxOut(..) )
import WalletAPI.UtxoStoreConfig
import Data.Aeson (FromJSON)
import Spectrum.Common.Persistence.Serialization
  ( serialize, deserializeM )
import Data.Conduit

type Store = Map.Map TxOutRef FullTxOut

data UtxoStore f = UtxoStore
  { putUtxos     :: Set.Set FullTxOut -> f ()
  , getUtxos     :: f (Set.Set FullTxOut)
  , dropUtxos    :: Set.Set TxOutRef -> f ()
  , containsUtxo :: TxOutRef -> f Bool
  }

mkPersistentUtxoStoreWithCache :: forall i f. (MonadIO i, MonadResource i, MonadIO f, MonadThrow f) => MakeLogging i f -> UtxoStoreConfig -> i (UtxoStore f)
mkPersistentUtxoStoreWithCache MakeLogging{.. } UtxoStoreConfig{..} = do
  logging <- forComponent "PersistentUtxoStore"
  storeT  <- liftIO $ newTVarIO mempty
  (_, db) <- Rocks.openBracket utxoStorePath
              Rocks.defaultOptions
                { Rocks.createIfMissing = createIfMissing
                }
  let
    get :: FromJSON a => ByteString -> f (Maybe a)
    get = (=<<) (mapM deserializeM) . Rocks.get db Rocks.defaultReadOptions
    put = Rocks.put db Rocks.defaultWriteOptions :: ByteString -> ByteString -> f ()
    delete = Rocks.delete db Rocks.defaultWriteOptions :: ByteString -> f ()
  pure $ attachTracing logging UtxoStore
    { putUtxos = \utxos ->
       ((\output@FullTxOut{fullTxOutRef} -> put (mkKey fullTxOutRef) (serialize output)) `traverse` toList utxos) >> put' storeT utxos
    , getUtxos = do
        inCache <- get' storeT
        if null inCache
        then Rocks.range db Nothing Nothing >>= (\case
               (Just utxoStream) -> do
                 rawUtxosList <- liftIO . sourceToList $ mapOutputMaybe id utxoStream
                 parsedUtxos  <- (\(_, rawUtxo) -> deserializeM rawUtxo) `traverse` rawUtxosList
                 pure $ Set.fromList parsedUtxos
               Nothing -> pure Set.empty
             )
        else pure inCache
    , dropUtxos = \refs ->
        (delete . mkKey) `traverse` toList refs >> drop' storeT refs
    , containsUtxo = \ref ->
        (get (mkKey ref) :: f (Maybe FullTxOut)) <&> isJust
    }

mkUtxoStore :: (MonadIO i, MonadIO f) => MakeLogging i f -> i (UtxoStore f)
mkUtxoStore MakeLogging{..} = do
  logging <- forComponent "UtxoStore"
  storeT  <- liftIO $ newTVarIO mempty
  pure $ attachTracing logging UtxoStore
    { putUtxos     = put' storeT
    , getUtxos     = get' storeT
    , dropUtxos    = drop' storeT
    , containsUtxo = contains' storeT
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

mkKey :: TxOutRef -> ByteString
mkKey = fromString . show

attachTracing :: Monad f => Logging f -> UtxoStore f -> UtxoStore f
attachTracing Logging{..} UtxoStore{..} =
  UtxoStore
    { putUtxos = \utxos -> do
        debugM $ "putUtxos " <> show utxos
        r <- putUtxos utxos
        debugM $ "putUtxos -> " <> show r
        pure r
    , getUtxos = do
        debugM @String "getUtxos"
        r <- getUtxos
        debugM $ "getUtxos -> " <> show r
        pure r
    , dropUtxos = \outRefs -> do
        debugM $ "dropUtxos " <> show outRefs
        r <- dropUtxos outRefs
        debugM $ "dropUtxos -> " <> show r
        pure r
    , containsUtxo = \ref -> do
        debugM $ "containsUtxo " <> show ref
        r <- containsUtxo ref
        debugM $ "containsUtxo -> " <> show r
        pure r
    }
