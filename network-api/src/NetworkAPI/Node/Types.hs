{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module NetworkAPI.Node.Types where

import Cardano.Api hiding (NetworkId)
import NetworkAPI.Types
import qualified RIO.Prelude as Int

newtype NetworkId = NetworkId Int deriving (Eq,Show,Ord)

instance Num NetworkId where
  (+) = \ (NetworkId a) (NetworkId b) -> NetworkId (a + b)
  (*) = \ (NetworkId a) (NetworkId b) -> NetworkId (a * b)
  abs =  \ (NetworkId a) -> NetworkId (abs a)
  signum = \ (NetworkId a) -> NetworkId (signum a)
  fromInteger = NetworkId . Int.fromInteger
  negate = \ (NetworkId a) -> NetworkId (Int.negate a)

instance Enum NetworkId where
  toEnum = NetworkId
  fromEnum = \(NetworkId a) -> a

data NetworkOperation f where
  SystemEnvOp :: f (LocalNodeConnectInfo CardanoMode -> f SystemEnvResult)   -> NetworkOperation f
  TxSubmition :: f (LocalNodeConnectInfo CardanoMode -> f TxSubmitionResult) -> NetworkOperation f

data OpearationResult where
   SystemEnvResult   :: SystemEnvResult   -> OpearationResult
   TxSubmitionResult :: TxSubmitionResult -> OpearationResult
   deriving (Show)

data TxSubmitionResult where
  Success :: TxSubmitionResult
  ErrorTxSubmitionResult :: String -> TxSubmitionResult

instance Show TxSubmitionResult where
  show Success = "Success for TxSubmitionResult"
  show (ErrorTxSubmitionResult desc) = "ErrorTxSubmitionResult for TxSubmitionResult. Error:" ++ desc

data SystemEnvResult where
  SuccessResult        :: SystemEnv -> SystemEnvResult
  ErrorSystemEnvResult :: String -> SystemEnvResult

instance Show SystemEnvResult where
  show (SuccessResult _) = "SuccessResult for SystemEnvResult"
  show (ErrorSystemEnvResult desc) = "ErrorSystemEnvResult for SystemEnv. Error:" ++ desc

data OpPriority = High | Low deriving (Show)