module CardanoTx.ToPlutus where

class ToPlutus a p where
  toPlutus :: a -> p
