module Spectrum.EventSource.Persistence.LedgerHistory
  ( LedgerHistory(..)
  , mkLedgerHistory
  , mkRuntimeLedgerHistory
  , TestStack(..)
  ) where

import RIO
  ( ByteString
  , ($>)
  , newIORef
  , readIORef
  , writeIORef
  , atomicModifyIORef'
  , (<&>)
  , isJust
  )

import qualified Data.Map as Map

import Control.Monad.IO.Class
  ( MonadIO )
import Control.Monad.Trans.Resource
  ( MonadResource )

import System.Logging.Hlog
  ( Logging (..), MakeLogging (..) )

import qualified Database.RocksDB as Rocks

import Spectrum.EventSource.Types
  ( ConcretePoint )
import Spectrum.EventSource.Persistence.Data.BlockLinks
  ( BlockLinks )
import Spectrum.EventSource.Persistence.Config
  ( LedgerStoreConfig (..) )
import Spectrum.Common.Persistence.Serialization (serialize, deserializeM)
import Control.Monad.Catch (MonadThrow)
import Data.List (uncons)
import GHC.IORef (IORef(IORef))
import Data.Foldable (find)

data TestStack m k = TestStack
  { push      :: k -> m ()
  , pop       :: m (Maybe k)
  , readFirst :: m (Maybe k)
  , exists    :: k -> m Bool
  }

mkTestStack :: (MonadIO f, MonadIO m, Eq k) => f (TestStack m k)
mkTestStack = do
  listRef <- newIORef []
  let size = 100
  return TestStack
    { push = \elem -> do
        atomicModifyIORef' listRef (\listStack ->
            if length listStack > size
              then
                let
                  (newStack, _) = splitAt (size `div` 2) listStack
                in (elem : newStack, ())
              else (elem : listStack, ())
          )
    , pop  =
        atomicModifyIORef' listRef (\listStack ->
          case uncons listStack of
            Nothing -> ([], Nothing)
            Just (element, newStack) -> (newStack, Just element)
        )
    , readFirst = readIORef listRef <&> (\list -> uncons list <&> fst)
    , exists = \key -> do
        list <- readIORef listRef
        pure $ elem key list
    }

data LedgerHistory m = LedgerHistory
  { setTip      :: ConcretePoint -> m ()
  , getTip      :: m (Maybe ConcretePoint)
  , putBlock    :: ConcretePoint -> BlockLinks -> m ()
  , getBlock    :: ConcretePoint -> m (Maybe BlockLinks)
  , pointExists :: ConcretePoint -> m Bool
  , dropBlock   :: ConcretePoint -> m Bool
  }

mkLedgerHistory
  :: (MonadIO f, MonadResource f, MonadIO m, MonadThrow m)
  => MakeLogging f m
  -> LedgerStoreConfig
  -> f (LedgerHistory m)
mkLedgerHistory MakeLogging{..} LedgerStoreConfig{..} = do
  logging <- forComponent "LedgerHistory"
  (_, db) <- Rocks.openBracket storePath
              Rocks.defaultOptions
                { Rocks.createIfMissing = createIfMissing
                }
  let
    readopts  = Rocks.defaultReadOptions
    writeopts = Rocks.defaultWriteOptions
  pure $ attachLogging logging LedgerHistory
    { setTip = Rocks.put db writeopts lastPointKey . serialize
    , getTip = Rocks.get db readopts lastPointKey >>= mapM deserializeM
    , putBlock = \point blk -> Rocks.put db writeopts (serialize point) (serialize blk)
    , getBlock = \point -> Rocks.get db readopts (serialize point) >>= mapM deserializeM
    , pointExists = \point -> Rocks.get db readopts (serialize point) <&> isJust
    , dropBlock = \point -> do
        let pkey = serialize point
        exists <- Rocks.get db readopts pkey <&> isJust
        if exists
          then Rocks.delete db writeopts pkey $> True
          else pure False
    }

-- | Runtime-only storage primarily for tests.
mkRuntimeLedgerHistory :: (MonadIO m, MonadThrow m) => m (LedgerHistory m)
mkRuntimeLedgerHistory = do
  tipsStack   <- mkTestStack
  blockStorage <- newIORef []
  store   <- newIORef mempty
  --logging <- forComponent "LedgerHistory"
  pure $ LedgerHistory
    { setTip = \p -> do
        push tipsStack p
        -- s <- readIORef store
        -- writeIORef store $ Map.insert lastPointKey (serialize p) s
    , getTip = do
        pop tipsStack
        -- s <- readIORef store
        -- mapM deserializeM $ Map.lookup lastPointKey s
    , putBlock = \point blk -> do
        atomicModifyIORef' blockStorage (\blockList ->
            if length blockList > 100
              then
                let
                  (newStorage, _) = splitAt (100 `div` 2) blockList
                in ((point, blk) : newStorage, ())
              else ((point, blk) : blockList, ())
          )
        -- push blocksStack (point, blk)
        -- s <- readIORef store
        -- writeIORef store $ Map.insert (serialize point) (serialize blk) s
    , getBlock = \point -> do
        s <- readIORef blockStorage
        pure $ find (\(testP, _) -> point == testP) s <&> snd
    , pointExists = \point -> do
        s <- readIORef store
        pure $ Map.member (serialize point) s
    , dropBlock = \point -> do
        atomicModifyIORef' blockStorage (\blockList ->
            (filter (\(testP, _) -> testP /= point) blockList, ())
          ) >> pure True
        -- s <- readIORef store
        -- let
        --   pkey   = serialize point
        --   exists = Map.member pkey s
        -- if exists
        --   then writeIORef store (Map.delete pkey s) $> True
        --   else pure False
    }

attachLogging :: Monad m => Logging m -> LedgerHistory m -> LedgerHistory m
attachLogging Logging{..} LedgerHistory{..} =
  LedgerHistory
    { setTip = \point -> do
        infoM $ "setTip " <> show point
        r <- setTip point
        infoM $ "setTip " <> show point <> " -> " <> show r
        pure r
    , getTip = do
        infoM @String "getTip"
        r <- getTip
        infoM $ "getTip -> " <> show r
        pure r
    , putBlock = \point blk -> do
        infoM $ "putBlock " <> show point
        r <- putBlock point blk
        infoM $ "putBlock " <> show point <> " -> " <> show r
        pure r
    , getBlock = \point -> do
        infoM $ "getBlock " <> show point
        r <- getBlock point
        infoM $ "getBlock " <> show point <> " -> " <> show r
        pure r
    , pointExists = \point -> do
        infoM $ "pointExists " <> show point
        r <- pointExists point
        infoM $ "pointExists " <> show point <> " -> " <> show r
        pure r
    , dropBlock = \point -> do
        infoM $ "dropBlock " <> show point
        r <- dropBlock point
        infoM $ "dropBlock " <> show point <> " -> " <> show r
        pure r
    }

lastPointKey :: ByteString
lastPointKey = "lastPoint"
