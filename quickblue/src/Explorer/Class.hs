module Explorer.Class where

class ToCardanoTx a target where
  toCardanoTx :: a -> target

class FromExplorer a b where
  parseFromExplorer :: a -> Maybe b