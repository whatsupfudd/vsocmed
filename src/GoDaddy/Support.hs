module GoDaddy.Support where

import Data.ByteString.Lazy as L
import Data.Aeson (FromJSON, ToJSON, Value (..), decode, parseJSON, toJSON, object, (.=), (.:), (.:?))


extractGError :: Maybe GError -> GError
extractGError (Just e) = e
extractGError Nothing = GError (Just "000") (Just "Error decoding response json error message") Nothing Nothing Nothing

maybeE :: Maybe m -> String -> String -> Either GError m
maybeE (Just m) _ _ = Right m
maybeE Nothing c s = Left $ GError (Just c) (Just s) Nothing Nothing Nothing

errorOrTrue :: Either GError L.ByteString -> Either GError Bool
errorOrTrue (Left e) = Left e
errorOrTrue (Right _) = Right True

maybeDecode :: FromJSON a => (Either GError L.ByteString) -> String -> String -> (Either GError a)
maybeDecode (Left e) _ _ = Left e
maybeDecode (Right r) c m = maybeE (decode r) c m

maybeDecodeL :: FromJSON a => (Either GError L.ByteString) -> String -> String -> (Either GError [a])
maybeDecodeL (Left e) _ _ = Left e
maybeDecodeL (Right r) c m = maybeE (decode r) c m



data GError = GError { erCode          :: Maybe String
                     , erMessage       :: Maybe String
                     , erRetryAfterSec :: Maybe Integer
                     , erName          :: Maybe String
                     , erFields        :: Maybe [Fields] } deriving (Show)

-- GError:

instance ToJSON GError where
  toJSON (GError c m r n f) =
    object [ "code" .= c
           , "message" .= m
           , "retryAfterSec" .= r
           , "name" .= n
           , "fields" .= f ]

instance FromJSON GError where
  parseJSON (Object v) =
    GError <$> v .:? "code"
           <*> v .:? "message"
           <*> v .:? "retryAfterSec"
           <*> v .:? "name"
           <*> v .:? "fields"
  parseJSON _ = fail "Error object not found"

data Fields = Fields { fiPath        :: String
                     , fiPathRelated :: Maybe String
                     , fiCode        :: String
                     , fiMessage     :: Maybe String } deriving (Show)

instance ToJSON Fields where
  toJSON (Fields p pr c m) =
    object [ "path" .= p
           , "pathRelated" .= pr
           , "code" .= c
           , "message" .= m ]

instance FromJSON Fields where
  parseJSON (Object v) =
    Fields <$> v .: "path"
           <*> v .:? "pathRelated"
           <*> v .: "code"
           <*> v .:? "message"
  parseJSON _ = fail "Fields object not found"


-- DomainTypes:
data Contact = Contact { nameFirst      :: String
                       , nameMiddle     :: Maybe String
                       , nameLast       :: String
                       , organization   :: Maybe String
                       , jobTitle       :: Maybe String
                       , email          :: String
                       , phone          :: String
                       , fax            :: Maybe String
                       , addressMailing :: AddressMailing } deriving (Show)

instance ToJSON Contact where
  toJSON (Contact f m l o j e p fa a) =
    object [ "nameFirst" .= f
           , "nameMiddle" .= m
           , "nameLast" .= l
           , "organization" .= o
           , "jobTitle" .= j
           , "email" .= e
           , "phone" .= p
           , "fax" .= fa
           , "addressMailing" .= a ]

instance FromJSON Contact where
  parseJSON (Object v) =
    Contact <$> v .: "nameFirst"
            <*> v .:? "nameMiddle"
            <*> v .: "nameLast"
            <*> v .:? "organization"
            <*> v .:? "jobTitle"
            <*> v .: "email"
            <*> v .: "phone"
            <*> v .:? "fax"
            <*> v .: "addressMailing"
  parseJSON _ = fail "Contact object not found"

data Contacts = Contacts { registrant :: Contact
                         , admin      :: Maybe Contact
                         , tech       :: Maybe Contact
                         , billing    :: Maybe Contact } deriving (Show)

instance ToJSON Contacts where
  toJSON (Contacts r a t b) =
    object [ "contactRegistrant" .= r
           , "contactAdmin" .= a
           , "contactTech" .= t
           , "contactBilling" .= b ]

instance FromJSON Contacts where
  parseJSON (Object v) =
    Contacts <$> v .: "contactRegistrant"
             <*> v .:? "contactAdmin"
             <*> v .:? "contactTech"
             <*> v .:? "contactBilling"
  parseJSON _ = fail "Contacts object not found"

data AddressMailing = AddressMailing { address1   :: String
                                     , address2   :: Maybe String
                                     , city       :: String
                                     , state      :: String
                                     , postalCode :: String
                                     , country    :: String } deriving (Show)

instance ToJSON AddressMailing where
  toJSON (AddressMailing a1 a2 c s p co) =
    object [ "address1" .= a1
           , "address2" .= a2
           , "city" .= c
           , "state" .= s
           , "postalCode" .= p
           , "country" .= co ]

instance FromJSON AddressMailing where
  parseJSON (Object v) =
    AddressMailing <$> v .: "address1"
                   <*> v .:? "address2"
                   <*> v .: "city"
                   <*> v .: "state"
                   <*> v .: "postalCode"
                   <*> v .: "country"
  parseJSON _ = fail "AddressMailing object not found"

data RealNameValidation = RealNameValidation { r_status :: Maybe String } deriving (Show)

instance ToJSON RealNameValidation where
  toJSON (RealNameValidation s) =
    object [ "status" .= s ]

instance FromJSON RealNameValidation where
  parseJSON (Object v) =
    RealNameValidation <$> v .:? "status"
  parseJSON _ = fail "RealNameValidation object not found"

data DomainSummary = DomainSummary { domainId            :: Float
                                   , domain              :: String
                                   , status              :: String
                                   , expires             :: Maybe String
                                   , expirationProtected :: Bool
                                   , holdRegistar        :: Bool
                                   , locked              :: Bool
                                   , privacy             :: Bool
                                   , renewAuto           :: Bool
                                   , renewable           :: Bool
                                   , renewDeadline       :: String
                                   , transferProtected   :: Bool
                                   , createdAt           :: String
                                   , authCode            :: Maybe String
                                   , nameServers         :: Maybe [String]
                                   , contactRegistrant   :: Maybe Contact
                                   , contactBill         :: Maybe Contact
                                   , contactAdmin        :: Maybe Contact
                                   , contactTech         :: Maybe Contact
                                   , realNameValidation  :: Maybe RealNameValidation
                                   , subAccountId        :: Maybe String} deriving (Show)

instance ToJSON DomainSummary where
  toJSON (DomainSummary id d s e ep hr l p ra r rd tp ca ac ns cr cb cad ct rnv sai) =
    object [ "domainId" .= id
           , "domain" .= d
           , "status" .= s
           , "expires" .= e
           , "expirationProtected" .= ep
           , "holdRegistrar" .= hr
           , "locked" .= l
           , "privacy" .= p
           , "renewAuto" .= ra
           , "renewable" .= r
           , "renewDeadline" .= rd
           , "transferProtected" .= tp
           , "createdAt" .= ca
           , "authCode" .= ac
           , "nameServers" .= ns
           , "contactRegistrant" .= cr
           , "contactBilling" .= cb
           , "contactAdmin" .= cad
           , "contactTech" .= ct
           , "realNameValidation" .= rnv
           , "subaccountId" .= sai]

instance FromJSON DomainSummary where
  parseJSON (Object v) =
    DomainSummary <$> v .: "domainId"
                  <*> v .: "domain"
                  <*> v .: "status"
                  <*> v .:? "expires"
                  <*> v .: "expirationProtected"
                  <*> v .: "holdRegistrar"
                  <*> v .: "locked"
                  <*> v .: "privacy"
                  <*> v .: "renewAuto"
                  <*> v .: "renewable"
                  <*> v .: "renewDeadline"
                  <*> v .: "transferProtected"
                  <*> v .: "createdAt"
                  <*> v .:? "authCode"
                  <*> v .:? "nameServers"
                  <*> v .:? "contactRegistrant"
                  <*> v .:? "contactBilling"
                  <*> v .:? "contactAdmin"
                  <*> v .:? "contactTech"
                  <*> v .:? "realNameValidation"
                  <*> v .:? "subaccountId"
  parseJSON _ = fail "DomainSummary object not found"


data DomainUpdate = DomainUpdate { dlocked       :: Maybe Bool
                                 , dnameServers  :: Maybe [String]
                                 , drenewAuto    :: Maybe Bool
                                 , dsubaccountId :: Maybe String } deriving (Show)

instance ToJSON DomainUpdate where
  toJSON (DomainUpdate l ns ra si) =
    object [ "locked" .= l
           , "nameServers" .= ns
           , "renewAuto" .= ra
           , "subaccountId" .= si ]

instance FromJSON DomainUpdate where
  parseJSON (Object v) =
    DomainUpdate <$> v .:? "locked"
                 <*> v .:? "nameServers"
                 <*> v .:? "renewAuto"
                 <*> v .:? "subaccountId"
  parseJSON _ = fail "DomainUpdate object not found"

data Consent = Consent { agreementKeys :: [String]
                       , agreedBy      :: String
                       , agreeAt       :: String } deriving (Show)

instance ToJSON Consent where
  toJSON (Consent ag ab aa) = object [ "agreementKeys" .= ag
                                     , "agreedBy" .= ab
                                     , "agreedAt" .= aa ]
instance FromJSON Consent where
  parseJSON (Object v) =
    Consent <$> v .: "agreementKeys"
            <*> v .: "agreedBy"
            <*> v .: "agreedAt"
  parseJSON _ = fail "Consent object not found"

data PrivacyPurchase = PrivacyPurchase { consent :: Consent } deriving (Show)

instance ToJSON PrivacyPurchase where
  toJSON (PrivacyPurchase c) = object [ "consent" .= c ]

instance FromJSON PrivacyPurchase where
  parseJSON (Object v) =
    PrivacyPurchase <$> v .: "consent"
  parseJSON _ = fail "PrivacyPurchase object not found"


data DomainPurchaseResponse = DomainPurchaseResponse { orderId   :: Integer
                                                     , itemCount :: Integer
                                                     , total     :: Integer
                                                     , currency  :: String } deriving (Show)

instance ToJSON DomainPurchaseResponse where
  toJSON (DomainPurchaseResponse oi ic t c) =
    object [ "orderId" .= oi
           , "itemCount" .= ic
           , "total" .= t
           , "currency" .= c]

instance FromJSON DomainPurchaseResponse where
  parseJSON (Object v) =
    DomainPurchaseResponse <$> v .: "orderId"
                           <*> v .: "itemCount"
                           <*> v .: "total"
                           <*> v .: "currency"
  parseJSON _ = fail "DomainPurchaseResponse object not found"


data DNSRecord = DNSRecord {
    kind :: String
   , name :: String
   , dataRec :: String
   , ttl :: Integer
   , priority :: Maybe String
   , service :: Maybe String
   , protocol :: Maybe String
   , port :: Maybe Integer
   , weight :: Maybe Integer
  } deriving (Show)

instance ToJSON DNSRecord where
  toJSON (DNSRecord t n d p ttl s pr po w) =
    object [ "type" .= t
           , "name" .= n
           , "data" .= d
           , "priority" .= p
           , "ttl" .= ttl
           , "service" .= s
           , "protocol" .= pr
           , "port" .= po
           , "weight" .= w ]

instance FromJSON DNSRecord where
  parseJSON (Object v) =
    DNSRecord <$> v .: "type"
              <*> v .: "name"
              <*> v .: "data"
              <*> v .: "ttl"
              <*> v .:? "priority"
              <*> v .:? "service"
              <*> v .:? "protocol"
              <*> v .:? "port"
              <*> v .:? "weight"
  parseJSON _ = fail "DNSRecord object not found"

data DomainRenew = DomainRenew { period :: Integer } deriving (Show)

instance ToJSON DomainRenew where
  toJSON (DomainRenew p) = object [ "period" .= p ]

instance FromJSON DomainRenew where
  parseJSON (Object v) = DomainRenew <$> v .: "period"


data DomainTransferIn = DomainTransferIn { transAuthCode  :: String
                                         , transPeriod    :: Maybe Integer
                                         , transRenewAuto :: Maybe Bool
                                         , transPrivacy   :: Maybe Bool
                                         , transConsent   :: Consent } deriving (Show)

instance ToJSON DomainTransferIn where
  toJSON (DomainTransferIn ac p ra pr c) =
    object [ "authCode" .= ac
           , "period" .= p
           , "renewAuto" .= ra
           , "privacy" .= pr
           , "consent" .= c ]

instance FromJSON DomainTransferIn where
  parseJSON (Object v) =
    DomainTransferIn <$> v .: "authCode"
                     <*> v .:? "period"
                     <*> v .:? "renewAuto"
                     <*> v .:? "privacy"
                     <*> v .: "consent"
  parseJSON _ = fail "DomainTransferIn object not found"

data LegalAgreement = LegalAgreement { agreementKey :: String
                                     , title        :: String
                                     , url          :: Maybe String
                                     , content      :: String } deriving (Show)

instance ToJSON LegalAgreement where
  toJSON (LegalAgreement ak t u c) =
    object [ "agreementKey" .= ak
           , "title" .= t
           , "url" .= u
           , "content" .= c ]

instance FromJSON LegalAgreement where
  parseJSON (Object v) =
    LegalAgreement <$> v .: "agreementKey"
                   <*> v .: "title"
                   <*> v .:? "url"
                   <*> v .: "content"
  parseJSON _ = fail "LegalAgreement object not found"


data DomainAvailableResponse = DomainAvailableResponse { availDomain   :: String
                                                       , available     :: Bool
                                                       , price         :: Maybe Integer
                                                       , availCurrency :: Maybe String
                                                       , availPeriod   :: Maybe Integer} deriving (Show)

instance ToJSON DomainAvailableResponse where
  toJSON (DomainAvailableResponse av a p ac ap) =
    object [ "domain" .= av
           , "available" .= a
           , "price" .= p
           , "currency" .= ac
           , "period" .= ap ]

instance FromJSON DomainAvailableResponse where
  parseJSON (Object v) =
    DomainAvailableResponse <$> v .: "domain"
                            <*> v .: "available"
                            <*> v .:? "price"
                            <*> v .:? "currency"
                            <*> v .:? "period"
  parseJSON _ = fail "DomainAvailableResponse object not found"

data IdentityDocumentCreate = IdentityDocumentCreate { identificationType   :: String
                                                     , identityDomain       :: String
                                                     , legalEntityName      :: String
                                                     , identificationNumber :: String
                                                     , image                :: String
                                                     , identityConcent      :: Consent } deriving (Show)

instance ToJSON IdentityDocumentCreate where
  toJSON (IdentityDocumentCreate it id len idn i idc) =
    object [ "identificationType" .= it
           , "domain" .= id
           , "legalEntityName" .= len
           , "image" .= i
           , "consent" .= idc ]

instance FromJSON IdentityDocumentCreate where
  parseJSON (Object v) =
    IdentityDocumentCreate <$> v .: "identificationType"
                           <*> v .: "domain"
                           <*> v .: "legalEntityName"
                           <*> v .: "identificationNumber"
                           <*> v .: "iamge"
                           <*> v .: "concent"
  parseJSON _ = fail "IdentityDocumentCreate object not found"

data DomainPurchase = DomainPurchase { purchaseDomain      :: String
                                     , purchaseConsent     :: Consent
                                     , purchasePeriod      :: Maybe Integer
                                     , purchaseNameServers :: Maybe [String]
                                     , purchaseRenewAuto   :: Maybe Bool
                                     , purchasePrivacy     :: Maybe Bool
                                     , purchaseRegistrant  :: Maybe Contact
                                     , purchaseAdmin       :: Maybe Contact
                                     , purchaseTech        :: Maybe Contact
                                     , purchaseBilling     :: Maybe Contact } deriving (Show)

instance ToJSON DomainPurchase where
  toJSON (DomainPurchase pd pc pp pns pr ppr pre pa pt pb) =
    object [ "domain" .= pd
           , "consent" .= pc
           , "period" .= pp
           , "nameServers" .= pns
           , "renewAuto" .= pr
           , "privacy" .= ppr
           , "contactRegistrant" .= pre
           , "contactAdmin" .= pa
           , "contactTech" .= pt
           , "contactBilling" .= pb ]

instance FromJSON DomainPurchase where
  parseJSON (Object v) =
    DomainPurchase <$> v .: "domain"
                   <*> v .: "consent"
                   <*> v .:? "period"
                   <*> v .:? "nameServers"
                   <*> v .:? "renewAuto"
                   <*> v .:? "privacy"
                   <*> v .:? "contactRegistrant"
                   <*> v .:? "contactAdmin"
                   <*> v .:? "contactTech"
                   <*> v .:? "contactBilling"
  parseJSON _ = fail "DomainPurchase object not found"

data SchemaDataType = SchemaDataType { dataType    :: String
                                     , dataRef     :: String
                                     , dataFormat  :: Maybe String
                                     , dataPattern :: Maybe String } deriving (Show)

instance ToJSON SchemaDataType where
  toJSON (SchemaDataType dt dr df dp) =
    object [ "type" .= dt
           , "$ref" .= dr
           , "format" .= df
           , "pattern" .= dp ]

instance FromJSON SchemaDataType where
  parseJSON (Object v) =
    SchemaDataType <$> v .: "type"
                   <*> v .: "$ref"
                   <*> v .:? "format"
                   <*> v .:? "pattern"
  parseJSON _ = fail "SchemaDataType object not found"

data SchemaProperties = SchemaProperties { propertyType         :: String
                                         , propertyRef          :: String
                                         , propertyItems        :: Maybe [SchemaDataType]
                                         , propertyRequired     :: Bool
                                         , propertyMaxItems     :: Maybe Integer
                                         , propertyMinItems     :: Maybe Integer
                                         , propertyDefaultValue :: Maybe String
                                         , propertyFormat       :: Maybe String
                                         , propertyPattern      :: Maybe String
                                         , propertyMaximum      :: Maybe Integer
                                         , propertyMinimum      :: Maybe Integer } deriving (Show)

instance ToJSON SchemaProperties where
  toJSON (SchemaProperties pt pr pi preq pmax pmin pdv pf pp pm pn) =
    object [ "type" .= pt
           , "$ref" .= pr
           , "items" .= pi
           , "required" .= preq
           , "maxItems" .= pmax
           , "minItems" .= pmin
           , "defaultValue" .= pdv
           , "format" .= pf
           , "pattern" .= pp
           , "maximum" .= pm
           , "minimum" .= pn ]

instance FromJSON SchemaProperties where
  parseJSON (Object v) =
    SchemaProperties <$> v .: "type"
                     <*> v .: "$ref"
                     <*> v .:? "items"
                     <*> v .: "required"
                     <*> v .:? "maxItems"
                     <*> v .:? "minItems"
                     <*> v .:? "defaultValue"
                     <*> v .:? "format"
                     <*> v .:? "pattern"
                     <*> v .:? "maximum"
                     <*> v .:? "minimum"
  parseJSON _ = fail "SchemaProperties object not found"

data Schema = Schema { schemaId         :: String
                     , schemaProperties :: [SchemaProperties]
                     , schemaRequired   :: [String]
                     , schemaModels     :: Schema } deriving (Show)

instance ToJSON Schema where
  toJSON (Schema sid sp sr sm) =
    object [ "id" .= sid
           , "properties" .= sp
           , "required" .= sr
           , "models" .= sm ]

instance FromJSON Schema where
  parseJSON (Object v) =
    Schema <$> v .: "id"
           <*> v .: "properties"
           <*> v .: "required"
           <*> v .: "models"

data DomainSuggestion = DomainSuggestion { suggestedDomain :: String } deriving (Show)

instance ToJSON DomainSuggestion where
  toJSON (DomainSuggestion sd) = object [ "domain" .= sd ]

instance FromJSON DomainSuggestion where
  parseJSON (Object v) = DomainSuggestion <$> v .: "domain"
  parseJSON _ = fail "DomainSuggestion object not found"

data TldSummary = TldSummary { tldName :: String
                             , tldType :: String } deriving (Show)

instance ToJSON TldSummary where
  toJSON (TldSummary n t) = object [ "name" .= n, "type" .= t ]

instance FromJSON TldSummary where
  parseJSON (Object v) = TldSummary <$> v .: "name" <*> v .: "type"
  parseJSON _ = fail "TldSummary object not found"
