module Main where

import Plutarch
import RIO
import qualified RIO as RIO
import Data.Text (Text)
import Plutarch.Evaluate (evaluateScript)
import Plutarch (ClosedTerm, compile)
import ErgoDex.Amm.Test
import ErgoDex.PContracts.PPool
import Explorer.Service
import Explorer.Config
import NetworkAPI.Config.NodeConfig
import SubmitAPI.Config
import ErgoDex.Amm.Test
import NetworkAPI.Service
import WalletAPI.TrustStore as TrustStore
import WalletAPI.Vault
import SubmitAPI.Service 
import Control.Monad.Trans.Resource
import qualified Data.Text as T

a :: Text
a = T.pack ""

main :: IO ()
main = runResourceT $ do
    --void $ print $ printTerm (pMkSwapValidator)
    -- _ <- print $ eval (merklizedPoolValidator # datum # redeemer # ctx)
    -- _ <- print $ eval (poolValidator # pPPoolConfig # pPPoolDepositRedeemer # ctx)
    -- _ <- print $ eval (poolValidator # pPPoolConfig # pPPoolRedeemRedeemer # ctx)
    let
        trustStore     = (mkTrustStore $ SecretFile "/home/timofey/development/haskell/instanceGenerator/keys/test.txt") :: TrustStore IO
    RIO.lift $ TrustStore.init trustStore $ KeyPass a
    let
        explorer       = (mkExplorer $ ExplorerConfig "0.0.0.0" 8084) :: Explorer IO
    vault <- RIO.lift $ mkVault explorer trustStore (KeyPass a)
    let
        network        = (mkNetwork (NodeConfig 8090 "136.243.21.170") explorer) :: Network IO
        submitService  = mkSubmitService network vault (TxAssemblyConfig Balance Cover)
    
    finilized <- RIO.lift $ finalizeTx submitService $ txCandidate
    RIO.lift $ print finilized
    RIO.lift $ pure ()