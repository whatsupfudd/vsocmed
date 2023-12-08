{-# LANGUAGE DerivingStrategies #-}

module Options.Cli where

import Data.Text (Text)
import Options.Applicative


data EnvOptions = EnvOptions {
    home :: Maybe Text
  }

data CliOptions = CliOptions {
  debug :: Maybe Int
  , configFile :: Maybe FilePath
  , job :: Maybe Command
 }
 deriving stock (Show)

data GlobalOptions = GlobalOptions {
  confPathGO :: String
  , debugGO :: String
  }

data Command =
  HelpCmd
  | VersionCmd
  | InitCmd Text Text Text
  | FetchCmd Text
  deriving stock (Show)

{- HERE: Additional structures for holding new command parameters:
Eg:
data ImportOpts = ImportOpts {
    taxonomy :: Text
    , path :: Text
  }
-}

parseCliOptions :: IO (Either String CliOptions)
parseCliOptions =
  Right <$> execParser parser

parser :: ParserInfo CliOptions
parser =
  info (helper <*> argumentsP) $
    fullDesc <> progDesc "vsocmed." <> header "vsocmed - ."


argumentsP :: Parser CliOptions
argumentsP = do
  buildOptions <$> globConfFileDef <*> hsubparser commandDefs
  where
    buildOptions :: GlobalOptions -> Command -> CliOptions
    buildOptions globs cmd =
      let
        mbConfPath = case globs.confPathGO of
          "" -> Nothing
          aValue -> Just aValue
        mbDebug = case globs.debugGO of
          "" -> Nothing
          aValue -> Just (read aValue :: Int)
      in
      CliOptions {
        debug = mbDebug
        , configFile = mbConfPath
        , job = Just cmd
      }


globConfFileDef :: Parser GlobalOptions
globConfFileDef =
  GlobalOptions <$>
    strOption (
      long "config"
      <> short 'c'
      <> metavar "vsocmedCONF"
      <> value ""
      <> showDefault
      <> help "Global config file (default is ~/.vsocmed/config.yaml)."
    )
    <*>
    strOption (
      long "debug"
      <> short 'd'
      <> metavar "DEBUGLVL"
      <> value ""
      <> showDefault
      <> help "Global debug state."
    )
  

commandDefs :: Mod CommandFields Command
commandDefs =
  let
    cmdArray = [
      ("help", pure HelpCmd, "Help about any command.")
      , ("version", pure VersionCmd, "Shows the version number of importer.")
      , ("init", initOpts, "Create a new website structure.")
      , ("fetch", fetchOpts, "List DNS records.")
      ]
    headArray = head cmdArray
    tailArray = tail cmdArray
  in
    foldl (\accum aCmd -> (cmdBuilder aCmd) <> accum) (cmdBuilder headArray) tailArray
  where
    cmdBuilder (label, cmdDef, desc) =
      command label (info cmdDef (progDesc desc))

initOpts :: Parser Command
initOpts =
  InitCmd <$> strArgument (metavar "ROOT" <> help "New website root path.")
    <*> strOption (
        long "concept"
        <> short 'c'
        <> metavar "CONCEPT"
        <> value ""
        <> showDefault
        <> help "Concept for the new website."
      )
    <*> strOption (
        long "bootfile"
        <> short 'b'
        <> metavar "BOOTFILE"
        <> value ""
        <> showDefault
        <> help "File containing the concept for the new website."
      )


fetchOpts :: Parser Command
fetchOpts =
  FetchCmd <$> strOption (
        long "filter"
        <> short 'f'
        <> metavar "FILTER"
        <> value ""
        <> showDefault
        <> help "Filter for domains."
      )
