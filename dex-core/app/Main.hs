module Main where

import Plutarch
import RIO
import Data.Text (Text)
import Plutarch.Evaluate (evaluateScript)
import Plutarch (ClosedTerm, compile)
import ErgoDex.Amm.Test
import ErgoDex.PContracts.PPool

main :: IO ()
main = do
    --void $ print $ printTerm (pMkSwapValidator)
    -- _ <- print $ eval (merklizedPoolValidator # datum # redeemer # ctx)
    -- _ <- print $ eval (poolValidator # pPPoolConfig # pPPoolDepositRedeemer # ctx)
    -- _ <- print $ eval (poolValidator # pPPoolConfig # pPPoolRedeemRedeemer # ctx)
    pure $ ()