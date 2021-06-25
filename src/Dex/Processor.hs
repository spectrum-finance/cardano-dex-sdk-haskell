module Dex.Processor where

import           Dex.Models

process :: FullTxOut -> Either (Operation a) Pool
process = undefined