module ErgoDex.Errors where

import ErgoDex.Contracts.Types

newtype TxCandidateCreationErr = TxCandidateCreationErr { errMsg :: String }

coinNotFoundError :: Coin a -> TxCandidateCreationErr
coinNotFoundError coin =
  TxCandidateCreationErr ( "Coin " ++ show coin ++ " not presented" )

incorrectLpAmountError :: TxCandidateCreationErr
incorrectLpAmountError =
	TxCandidateCreationErr ( "Incorrect lp value" )