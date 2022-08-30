module Explorer.Class where

class ToCardanoTx a target where
  toCardanoTx :: a -> target
