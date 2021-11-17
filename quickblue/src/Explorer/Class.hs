module Explorer.Class where

class ToCardanoTx ctx a where
  toCardanoTx :: a -> ctx
