module CardanoTx.ToPlutus where

class ToPlutus a p where
  toPlutus :: a -> p

class FromPlutus a p where
  fromPlutus :: p -> a
