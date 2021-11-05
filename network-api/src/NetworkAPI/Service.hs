module NetworkAPI.Service where

import NetworkAPI.Env

data NetworkParams f = NetworkParams
  { getSystemEnv :: f SystemEnv
  }
