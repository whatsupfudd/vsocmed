module SiteDefinition.Logic where

import System.Directory (createDirectoryIfMissing)

import qualified Options.Runtime as Rto

import SiteDefinition.Types


makeSite :: Rto.RunOptions -> FilePath -> IO (Either String ())
makeSite rtOpts cliRootDir =
  let
    rootPath = case cliRootDir of
      "" -> rtOpts.rootDir
      _ -> cliRootDir
    coreContent = [
        Assets, Config, Content, Data, I18n
        , Layouts, Public, Resources, Static, Themes
      ]
  in do
  mapM_ (\d -> createDirectoryIfMissing False (rootPath <> "/" <> (kindToName d))) coreContent
  pure $ Right ()
