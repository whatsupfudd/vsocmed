module Options.Runtime (defaultRun
    , RunOptions (..), GoDadAuth (..), OpenAiAuth (..)
  ) where

import Data.Text (Text)

data RunOptions = RunOptions {
    debug :: Int
    , goDaddy :: Maybe GoDadAuth
    , rootDir :: String
    , openAI :: Maybe OpenAiAuth
  }
  deriving (Show)


data GoDadAuth = GoDadAuth {
    ident :: String
    , secret :: String
    , customerID :: String
  }
  deriving (Show)

data OpenAiAuth = OpenAiAuth {
    key :: Text
  }
  deriving (Show)

defaultRun :: RunOptions
defaultRun =
  RunOptions {
    debug = 0
    , goDaddy = Nothing
    , rootDir = "/tmp"
    , openAI = Nothing
  }
