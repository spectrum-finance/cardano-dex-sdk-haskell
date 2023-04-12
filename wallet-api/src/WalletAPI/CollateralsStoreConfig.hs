module WalletAPI.CollateralsStoreConfig
  ( CollateralsStoreConfig(..)
  ) where

import GHC.Generics
  ( Generic )
import Dhall
  ( FromDhall )

data CollateralsStoreConfig = CollateralsStoreConfig
   { collateralsStorePath   :: FilePath
   , createIfMissing        :: Bool
   -- There no necessary to store all users utxos,
   -- because we are using them for collaterals.
   , minimumAdaCapacity     :: Integer
   } deriving (Generic, FromDhall)