module Dex.Processor where

import           Dex.Models
import           Prelude

process :: FullTxOut -> Either (Operation a) Pool
process = undefined