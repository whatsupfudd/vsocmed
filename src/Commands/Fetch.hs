module Commands.Fetch where

import Data.Text (Text, pack, unpack)
import qualified Data.Text as DT
import Data.Text.Encoding (encodeUtf8)
import Control.Exception
import Control.Monad (foldM)
import Network.HTTP.Client (newManager, parseRequest, httpLbs
                          , responseStatus, responseBody, Request (..))
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.HTTP.Types.Status (statusCode)
import Data.Aeson (FromJSON, ToJSON, Value (..), decode, parseJSON, toJSON, object, (.=), (.:), (.:?))
import Data.ByteString.Lazy as L


import qualified Options.Runtime as Rto

import GoDaddy.Support


fetchCmd :: Maybe Text -> Rto.RunOptions -> IO ()
fetchCmd mbFilter rtOpts = do
  -- putStrLn $ "fetch: starting."
  case rtOpts.goDaddy of
    Nothing ->
      putStrLn $ "@[fetchCmd] no GoDaddy identification available."
    Just goDad ->
      let
        authGD = AuthGD { ident = goDad.ident, secret = goDad.secret, customerID = goDad.customerID }
        accessor = 
          let
            (v1, v2) = domainUrl
          in
          AccessorGD { endpointV1 = v1, endpointV2= v2, authentication = authGD }
      in do
      eiRez <- getDomains accessor
      case eiRez of
        Left err -> do
          putStrLn $ "@[fetchCmd] err: " <> show err
        Right domains -> do
          {-
            putStrLn $ "@[fetchCmd] got result: " <> show result
            mapM_ (inquireDomain accessor) result
          -}
          domRez <- mapM (filterDomain accessor) domains
          mapM_ (\r ->
              case r of
                Left errMsg -> putStrLn errMsg
                Right aRez -> case aRez of
                  Nothing -> pure ()
                  Just (d, c) -> putStrLn $ "domain " <> d <> " is parked."
            ) domRez
          
  where
  inquireDomain :: AccessorGD -> DomainSummary -> IO ()
  inquireDomain accessor domain = do
    eiRez <- getDomainRecords accessor domain.domain (Just "A") Nothing
    case eiRez of
      Left err -> putStrLn $ "@[fetchCmd] get domain records err: " <> show err
      Right records -> do
        putStrLn $ "@[fetchCmd] domain: " <> domain.domain <> ":"
        mapM_ describeRecord records
        case records of
          [] -> pure ()
          _ -> putStrLn "---------"

  describeRecord :: DNSRecord -> IO ()
  describeRecord aDnsRec =
    putStrLn $ "@[fetchCmd] type:  " <> aDnsRec.kind <> ", name: " <> aDnsRec.name
        <> ", data: " <> aDnsRec.dataRec <> "."

  filterDomain :: AccessorGD -> DomainSummary -> IO (Either String (Maybe (String, DNSRecord)))
  filterDomain accessor domain = do
    eiRez <- getDomainRecords accessor domain.domain (Just "A") Nothing
    case eiRez of
      Left err -> pure . Left $ "@[fetchCmd] get domain records err: " <> show err
      Right records ->
        pure $ Prelude.foldl (\accum c ->
              if testCandidate c then
                case accum of
                  Right Nothing -> Right $ Just (domain.domain, c)
                  Right (Just (d,p)) -> Left $ "@[filterDomain] duplicate for " <> domain.domain
                          <> ", rec: " <> show p <> " vs " <> show c
                  Left _ -> accum
              else
                accum
            ) (Right Nothing) records

  testCandidate :: DNSRecord -> Bool
  testCandidate aRecord =
    aRecord.kind == "A" && aRecord.name == "@" && aRecord.dataRec == "Parked"


data ApiVersion =
  V1
  | V2 String

data AccessorGD = AccessorGD {
    endpointV1 :: String
    , endpointV2 :: String
    , authentication :: AuthGD
  } deriving (Show)


data AuthGD = AuthGD {
    ident :: String
    , secret :: String
    , customerID :: String
  } deriving (Show)

type Domain = String
type Type = String
type Name = String

domainUrl = 
  let
    debug = 0
  in
  if debug /= 0 then
    ("https://api.ote-godaddy.com/v1/", "https://api.ote-godaddy.com/v2/customers/")
  else
    ("https://api.godaddy.com/v1/", "https://api.godaddy.com/v2/customers/")


-- | Transform an authentication data type to a Text type. This value is then used in sendRequest to build http request headers.
--
packAuth :: AuthGD -> Text
packAuth auth = DT.pack $ "sso-key " ++ auth.ident ++ ":" ++ auth.secret

getRequest :: AccessorGD -> ApiVersion -> Maybe String -> IO (Either GError L.ByteString)
getRequest accessor version postfix = do
  sendRequest accessor version "GET" postfix

sendRequest :: AccessorGD -> ApiVersion -> String -> Maybe String -> IO (Either GError L.ByteString)
sendRequest accessor version kind postfix =
  let
    fullPath = case version of
      V1 -> case postfix of
          Nothing -> accessor.endpointV1 <> "domains/?statuses=ACTIVE"
          Just aString -> accessor.endpointV1 <> "domains/" <> aString
      V2 customerID ->
        case postfix of
          Nothing -> accessor.endpointV2 <> customerID <> "/domains"
          Just aString -> accessor.endpointV2 <> customerID <> "/domains/" <> aString
  in do
  manager <- newManager tlsManagerSettings
  initialRequest <- parseRequest $ fullPath
  -- putStrLn $ "@[sendRequest] hitting: " <> fullPath
  let request = initialRequest { 
           method = encodeUtf8 $ DT.pack kind
           , requestHeaders = [ ("Authorization", encodeUtf8 $ packAuth accessor.authentication) ]
               <> case version of
                   V1 -> [ ("X-Shopper-Id", encodeUtf8 $ DT.pack accessor.authentication.customerID)  ]
                   V2 anID -> []
         }
  response <- httpLbs request manager
  if (statusCode $ responseStatus response) == 200
    then return $ Right $ responseBody response
    else return $ Left $ extractGError (decode $ responseBody response)

--
-- Domain methods
--

-- | Retrieve a list of Domains for the specified Shopper
getDomains :: AccessorGD -> IO (Either GError [DomainSummary])
getDomains accessor =
  let
    -- modeRequest = (V2 accessor.authentication.customerID)
    modeRequest = V1
  in
  getRequest accessor modeRequest  Nothing >>= return . (\x -> maybeDecode x "000" "error extracting DomainSummary")


-- | Retrieve details for the specified Domain
getDomain :: AccessorGD -> Domain -> IO (Either GError DomainSummary)
getDomain accessor domain =
  getRequest accessor V1 (Just domain)
    >>= return . (\x -> maybeDecode x "000" "error extracting DomainSummary")


-- | Retrieve DNS Records for the specified Domain, optionally with the specified Type and/or Name
getDomainRecords :: AccessorGD -> Domain -> Maybe Type -> Maybe Name -> IO (Either GError [DNSRecord])
getDomainRecords accessor domain (Just t) (Just n) = do
  rez <- getRequest accessor V1 (Just $ domain ++ "/records/" ++ t ++ "/" ++ n)
  -- putStrLn $ "@[getDomainRecords] response: " <> show rez
  return $ maybeDecodeL rez "000" "error extracting DNSRecord"
getDomainRecords accessor domain (Just t) _ = do
  rez <- getRequest accessor V1 (Just $ domain ++ "/records/" ++ t)
  -- putStrLn $ "@[getDomainRecords] response: " <> show rez
  return $ maybeDecodeL rez "000" "error extracting DNSRecord"
getDomainRecords accessor domain _ (Just n) = do
  rez <- getRequest accessor V1 (Just $ domain ++ "/records/" ++ n)
  -- putStrLn $ "@[getDomainRecords] response: " <> show rez
  return $ maybeDecodeL rez "000" "error extracting DNSRecord"
getDomainRecords accessor domain _ _ = do
  rez <- getRequest accessor V1 (Just $ domain ++ "/records")
  -- putStrLn $ "@[getDomainRecords] response: " <> show rez
  return $ maybeDecodeL rez "000" "error etracting DNSRecord"


-- | Retrieve the legal agreement(s) required to purchase the specified TLD and add-ons
getLegalAgreements :: AccessorGD -> IO (Either GError [LegalAgreement])
getLegalAgreements accessor = getRequest accessor V1 (Just $ "agreements") >>= return . (\x -> maybeDecodeL x "000" "error extracting LegalAgreements")

-- | Retrieve availability about a given domain.
getAvailability :: AccessorGD -> String -> IO (Either GError DomainAvailableResponse)
getAvailability accessor aDomain = getRequest accessor V1 (Just $ "available?domain=" <> aDomain) >>= return . (\x -> maybeDecode x "000" "Error on availability.")
