module Commands.Init where

import Data.Text (Text, pack, unpack)
import qualified Data.Text as DT
import qualified Data.Vector as V
import Control.Exception
import Network.HTTP.Client (newManager, ManagerSettings (..), responseTimeoutMicro)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Data.Time.Clock.POSIX (getPOSIXTime)
import OpenAI.Client

import qualified Options.Runtime as Rto

import SiteDefinition.Logic (makeSite)

data BootMode =
  BootFromText Text
  | BootFromPath String


initCmd :: Text -> Text -> String -> Rto.RunOptions -> IO ()
initCmd rootPath cliConcept bootfile rtOpts = do
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
      Right aConcept -> do
        -- putStrLn $ "@[initCmd] doing: " <> unpack aConcept
        rez <- makeSite rtOpts (unpack rootPath)
        case rez of
          Left errMsg -> putStrLn $ "@[initCmd] makeSite err: " <> errMsg
          Right _ -> do
            case rtOpts.openAI of
              Nothing ->
                putStrLn $ "@[initCmd] no OpenAI key to operate with."
              Just aVal -> do
                runAI aVal.key aConcept
                pure ()
      Left anErr ->
       putStrLn $ "@[initCmd] error: " <> anErr


runAI :: DT.Text -> DT.Text -> IO ()
runAI apiKey concept = do
  manager <- newManager (tlsManagerSettings { managerResponseTimeout = (responseTimeoutMicro (240 * 1000000))})
  let
    client = makeOpenAIClient apiKey manager 4
    gpt4ID = EngineId "gpt-4"
    turbo35ID = EngineId "gpt-3.5-turbo"
  -- result <- listEngines client
  rezA <- getEngine client gpt4ID
  case rezA of
    Left err -> putStrLn $ "@[run] getEngine err: " <> show err
    Right _ -> do
      rezB <- getEngine client turbo35ID
      case rezB of
        Left err -> putStrLn $ "@[run] getEngine err: " <> show err
        Right _ -> do
          stepsForResult client turbo35ID concept
          stepsForResult client gpt4ID concept


stepsForResult :: OpenAIClient -> EngineId -> DT.Text -> IO ()
stepsForResult client modelID concept = do
  let
    (EngineId modelName) = modelID
  putStrLn $ "@[runAI] " <> DT.unpack modelName <> ":"
  startQ <- (round . (* 1000)) <$> getPOSIXTime
  rezA <- doComplete client (makeConceptMsg modelID concept)
  endQ <- (round . (* 1000)) <$> getPOSIXTime
  putStrLn $ "@[runAI] exec time: " <> show (endQ - startQ)
  case rezA of
    Nothing -> putStrLn $ "@[runAI] no reply!" 
    Just firstResult -> do
      rezB <- doComplete client (makeJsonMsg modelID firstResult)
      case rezB of
        Nothing -> putStrLn $ "@[runAI] no json!"
        Just jsonTxt ->
          putStrLn $ "@[runAI] json: " <> DT.unpack jsonTxt


doComplete :: OpenAIClient -> ChatCompletionRequest -> IO (Maybe Text)
doComplete client chatMsg = do
  rezB <- completeChat client chatMsg
  case rezB of
    Left err -> do
      putStrLn $ "@[run] completeText err: "  <> show err
      pure Nothing
    Right chatCompletion ->
      let
        choices = chrChoices chatCompletion
      in
        case choices of
          [] -> do
            putStrLn "@[run] no choice."
            pure Nothing
          h : [] -> pure h.chchMessage.chmContent
          h : cs -> do
            putStrLn $ "@[run] rez 0: " <> show h <> " of " <> show (length choices) <> " choices."
            pure h.chchMessage.chmContent


showChoice :: TextCompletionChoice -> String
showChoice choice =
  let
    reply = tccText choice
    stopping = tccFinishReason choice
  in
  (DT.unpack reply) <> " (" <> show stopping <> ")"


makeConceptMsg :: EngineId -> Text -> ChatCompletionRequest
makeConceptMsg (EngineId engineID) prompt =
  ChatCompletionRequest { 
     chcrModel = ModelId engineID
     , chcrMessages = [
         ChatMessage {
           chmRole = "assistant"
           , chmContent = Just "Hi, I am a professional website designer, how can I help you creating new and interesting websites?"
           , chmFunctionCall = Nothing, chmName = Nothing
          }
         , ChatMessage {
           chmRole = "user"
           , chmContent = Just $ "Hi, I need to create a new website that can " <> prompt
           , chmFunctionCall = Nothing, chmName = Nothing
          }
         , ChatMessage {
           chmRole = "assistant"
           , chmContent = Just "I can start by providing a detailed project overview of the website that will instruct the implementation team on the main elements and targets of the work ahead."
           , chmFunctionCall = Nothing, chmName = Nothing
          }
         , ChatMessage {
           chmRole = "assistant"
           , chmContent = Just $ "I'll use the following document structure to describe the project overview: Purpose, Stucture & Features "
               <> "(sub-sectiions: Homepage, Forum, Blog section, Tutorial & Guides, Reources, Community Spotlight, Event Calendar, Integration & Functionality; each sub-section enumerates a few key features)"
               <> ", Design Aesthetics, End Note."
           , chmFunctionCall = Nothing, chmName = Nothing
          }
         , ChatMessage {
           chmRole = "user"
           -- , chmContent = Just "Yes that sounds great. Please provide the result in the form of a JSON structure for easy parsing by an automatic project analysis software."
           , chmContent = Just "Yes that sounds great, please go ahead."
           , chmFunctionCall = Nothing, chmName = Nothing
          }
       ]
     , chcrTemperature = Just 0.7
     , chcrTopP = Nothing
     , chcrN = Nothing
     , chcrStream = Nothing
     , chcrStop = Nothing
     , chcrMaxTokens = Nothing
     , chcrPresencePenalty = Nothing
     , chcrFrequencyPenalty = Nothing
     , chcrLogitBias = Nothing
     , chcrUser = Nothing
     , chcrFunctions = Nothing
  }


makeJsonMsg :: EngineId -> Text -> ChatCompletionRequest
makeJsonMsg (EngineId engineID) prompt =
  ChatCompletionRequest { 
     chcrModel = ModelId engineID
     , chcrMessages = [
         ChatMessage {
           chmRole = "assistant"
           , chmContent = Just $ "Here is the result with markdown formatting:\n" <> prompt
           , chmFunctionCall = Nothing, chmName = Nothing
          }
         , ChatMessage {
           chmRole = "user"
           , chmContent = Just $ "Thanks, please transform that text into a JSON structure that a project analysis software can parse easily." <> prompt
           , chmFunctionCall = Nothing, chmName = Nothing
          }
       ]
     , chcrTemperature = Just 0.7
     , chcrTopP = Nothing
     , chcrN = Nothing
     , chcrStream = Nothing
     , chcrStop = Nothing
     , chcrMaxTokens = Nothing
     , chcrPresencePenalty = Nothing
     , chcrFrequencyPenalty = Nothing
     , chcrLogitBias = Nothing
     , chcrUser = Nothing
     , chcrFunctions = Nothing
  }