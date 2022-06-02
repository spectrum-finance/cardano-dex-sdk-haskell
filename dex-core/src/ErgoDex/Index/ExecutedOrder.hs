module CardanoTx.ExecutedOrder where

import ErgoDex.Amm.Orders

-- data OrderType = Swap | Redeem | Deposit

data ExecutedSwap = ExecutedSwap
  { swapCfg :: Swap
  , actualQuote :: Amount Quote
  , outputId :: Text
  , inputId :: Text
  }

class FromExplorer a where
  parseFromExplorer :: FullTxOut -> Maybe (OnChain a)