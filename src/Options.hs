module Options  (
  module Options.Cli
  , module Options.ConfFile
  , module Options.Runtime
  , mergeOptions
 )
where

import qualified Data.Text as DT
import qualified Data.Text.Encoding as DT

import Options.Cli
import Options.ConfFile
import Options.Runtime
import qualified Options.Runtime as Rt


mergeOptions :: CliOptions -> FileOptions -> EnvOptions -> RunOptions
mergeOptions cli file env =
  -- TODO: put proper priority filling of values for the Runtime Options.
  let
    defO = defaultRun
    -- Update from config file:
    fileO =
      let
        dbgO = case file.debug of
          Nothing -> defO
          Just aVal -> defO { debug = aVal } :: RunOptions
        goDad0 = case file.goDaddy of
          Nothing -> dbgO
          Just aVal ->
            let
              fIdent = case aVal.ident of
                Nothing -> ""
                Just aVal -> aVal
              fSecret = case aVal.secret of
                Nothing -> ""
                Just aVal -> aVal
              fCustID = case aVal.customer of
                Nothing -> ""
                Just aVal -> aVal
            in
            dbgO { goDaddy = Just $ Rt.GoDadAuth { ident = fIdent, secret = fSecret, customerID = fCustID }} :: RunOptions
        root0 = case file.rootDir of
          Nothing -> goDad0
          Just aPath -> goDad0 { rootDir = aPath } :: RunOptions
        openAI0 = case file.openAI of
          Nothing -> root0
          Just aVal -> case aVal.key of
            Nothing -> root0
            Just aTxt -> root0 { openAI = Just . Rt.OpenAiAuth $ DT.pack aTxt } :: RunOptions
      in
      openAI0
    -- TODO: update from CLI options
    cliO = case cli.debug of
      Nothing -> fileO
      Just aVal -> fileO { debug = aVal } :: RunOptions
    -- TODO: update from ENV options
    envO = cliO
  in 
  envO
