module Commands.Init where

import qualified Options.Runtime as Rto
import Data.Text (Text, pack, unpack)
import Control.Exception


data BootMode =
  BootFromText Text
  | BootFromPath String


initCmd :: Text -> Text -> String -> Rto.RunOptions -> IO ()
initCmd rootPath cliConcept bootfile _ = do
  putStrLn $ "init: starting."
  if cliConcept == "" && bootfile == "" then
    putStrLn $ "You need to provide at least a cliConcept on the CLI or a bootfile to use."
  else
    let
      bootControl =
        if cliConcept /= "" then
          BootFromText cliConcept
        else
          BootFromPath $ bootfile
    in do
    rezConcept <-
      case bootControl of
        BootFromText aText -> pure $ Right aText
        BootFromPath aPath -> do
          rezRead <- (Right <$> readFile aPath) `catch` (
              \e -> const (pure . Left $ show e :: IO (Either String String)) (e :: IOException)
            )
          case rezRead of
            Left anErr -> pure $ Left anErr
            Right aString -> pure . Right $ pack aString
    case rezConcept of
      Right aConcept ->
        putStrLn $ "@[initCmd] doing: " <> unpack aConcept
      Left anErr ->
       putStrLn $ "@[initCmd] error: " <> anErr
