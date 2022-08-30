module ErgoDex.Amm.Scripts where

import Ledger
import Ledger.Typed.Scripts.Validators

import ErgoDex.Contracts.OffChain
import ErgoDex.Contracts.Proxy.OffChain

poolScript :: Validator
poolScript = validatorScript poolInstance

poolAddress :: Address
poolAddress = validatorAddress poolInstance

swapScript :: Validator
swapScript = validatorScript swapInstance

depositScript :: Validator
depositScript = validatorScript depositInstance

redeemScript :: Validator
redeemScript = validatorScript redeemInstance
