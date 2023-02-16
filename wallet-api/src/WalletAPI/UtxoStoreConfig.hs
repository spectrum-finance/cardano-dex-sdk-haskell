module WalletAPI.UtxoStoreConfig
  ( UtxoStoreConfig(..)
  ) where

import GHC.Generics
  ( Generic )
import Dhall
  ( FromDhall )

data UtxoStoreConfig = UtxoStoreConfig
   { utxoStorePath   :: FilePath
   , createIfMissing :: Bool
   } deriving (Generic, FromDhall)