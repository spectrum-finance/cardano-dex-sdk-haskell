module Api.Backend.EffectsGenerator where

import           Wallet.Effects (WalletEffect(..))

genWalletEffect :: WalletEffect a
genWalletEffect = BalanceTx