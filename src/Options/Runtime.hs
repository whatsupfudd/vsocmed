module Options.Runtime (defaultRun, RunOptions (..)) where
-- import Data.Int (Int)

import Data.Text (Text)

data RunOptions = RunOptions {
    debug :: Int
    -- HERE: Add additional vars for providing runtime parameters:
    -- Eg: , root :: Text
  }
  deriving (Show)

defaultRun :: RunOptions
defaultRun =
  RunOptions {
    debug = 0
  }
