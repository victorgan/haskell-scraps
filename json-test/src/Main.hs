{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}
import Data.Aeson
import Data.Aeson.Types
import Data.List as List
import Data.Char as Char
import qualified Data.Text as T
import qualified Data.ByteString.Lazy as B
import GHC.Generics

-------------------------------------------------------------------------------
-- Data Types
-------------------------------------------------------------------------------
data JsonHeader = JsonHeader 
    { create :: !HeaderInner 
    } deriving (Show,Generic)
instance ToJSON JsonHeader where toJSON = genericToJSON defaultOptions

data JsonDelete = JsonDelete 
    { delete :: !HeaderInner 
    } deriving (Show,Generic)
instance ToJSON JsonDelete where toJSON = genericToJSON defaultOptions

data HeaderInner = HeaderInner 
    { _index :: !EsIndex
    , _type  :: !EsActivityType
    , _id    :: !EsId
    } deriving (Show,Generic)
instance ToJSON HeaderInner where toJSON = genericToJSON defaultOptions

data JsonContentActivity = 
      ActivityAbnormal 
        { isPrivileged  :: !Integer -- user fields
        , objectId      :: !T.Text
        , puid          :: !T.Text
        , userName      :: !T.Text
        , tenantId              :: !T.Text -- mandatory fields
        , timeStamp             :: !T.Text
        , upn                   :: !T.Text
        , userInfoResolveState  :: !Integer
        -- , userInfoResolveTime   :: !T.Text
        -- , previousIpAddress             :: !T.Text
        -- , previousCityStateCountry      :: !T.Text
        -- , callerIp                      :: !T.Text
        -- , location                      :: !T.Text
        -- , previousLocationTimeStamp     :: !T.Text
        -- , customerUseFullNessFeedback   :: !Integer
        -- , insertionTime                 :: !T.Text
        } 
    | ActivityBlacklist 
        { isPrivileged  :: !Integer
        , objectId      :: !T.Text
        , puid          :: !T.Text
        , userName      :: !T.Text
        , tenantId              :: !T.Text -- mandatory fields
        , timeStamp             :: !T.Text
        , upn                   :: !T.Text
        , userInfoResolveState  :: !Integer
        -- , city                          :: !T.Text
        -- , cityStateCountry              :: !T.Text
        -- , country                       :: !T.Text   
        -- , customerUseFullNessFeedback   :: !Integer
        -- , insertionTime                 :: !T.Text
        -- , ipAddress                     :: !T.Text 
        -- , loginCount                    :: !Integer
        -- , userDomain                    :: !T.Text
        -- , userInfoResolveTime           :: !T.Text
        } 
    | ActivitySuspicious 
        { isPrivileged  :: !Integer
        , objectId      :: !T.Text
        , puid          :: !T.Text
        , userName      :: !T.Text
        , tenantId              :: !T.Text -- mandatory fields
        , timeStamp             :: !T.Text
        , upn                   :: !T.Text
        , userInfoResolveState  :: !Integer
        -- , ipAddress                     :: !T.Text
        -- , cityStateCountry              :: !T.Text
        -- , loginCount                    :: !Integer
        -- , insertionTime                 :: !T.Text
        } 
    | ActivityLeaked 
        { isPrivileged  :: !Integer
        , objectId      :: !T.Text
        , puid          :: !T.Text
        , userName      :: !T.Text
        , tenantId              :: !T.Text
        , timeStamp             :: !T.Text
        , upn                   :: !T.Text
        , userInfoResolveState  :: !Integer
        } 
    | ActivityFamiliar
        { isPrivileged  :: !Integer
        , objectId      :: !T.Text
        , puid          :: !T.Text
        , userName      :: !T.Text
        , tenantId              :: !T.Text
        , timeStamp             :: !T.Text
        , upn                   :: !T.Text
        , userInfoResolveState  :: !Integer
        } 
    | ActivityInfected 
        { isPrivileged  :: !Integer
        , objectId      :: !T.Text
        , puid          :: !T.Text
        , userName      :: !T.Text
        , tenantId              :: !T.Text
        , timeStamp             :: !T.Text
        , upn                   :: !T.Text
        , userInfoResolveState  :: !Integer
        } 
    deriving (Show,Generic)

-- Hack: hide tag for sum datatype by setting its field to TenantId, which is
-- then overwritten. Also capitalize first letter as record names must start
-- with lowercase.
instance ToJSON JsonContentActivity 
    where toJSON = genericToJSON defaultOptions { fieldLabelModifier = capitalizeFirst
                                                , sumEncoding = TaggedObject 
                                                    { tagFieldName      = "TenantId"
                                                    , contentsFieldName = "contents"
                                                    }
                                                }
capitalizeFirst :: String -> String
capitalizeFirst (x:xs) = (Char.toUpper x):xs
capitalizeFirst [] = []

data ActivityType = Abnormal
                  | Blacklist
                  | Familiar
                  | Infected
                  | Leaked
                  | Suspicious
                    deriving Enum

-------------------------------------------------------------------------------
-- Type Synonyms
-------------------------------------------------------------------------------
type ActivityNumber = Integer
type ActivityParameters = (TenantId, ActivityType, UserNumber, ActivityNumber)
type TenantId = T.Text
type UserNumber = Integer


type EsActivityType = T.Text
type EsId = T.Text
type EsIndex = T.Text

-------------------------------------------------------------------------------
-- Header Making
-------------------------------------------------------------------------------
toText :: Integer -> T.Text
toText = T.pack . show

makeHeader :: EsIndex -> EsActivityType -> EsId -> JsonHeader
makeHeader _index _type _id = JsonHeader $ HeaderInner _index _type _id

makeDelete :: EsIndex -> EsActivityType -> EsId -> JsonDelete
makeDelete _index _type _id = JsonDelete $ HeaderInner _index _type _id

-------------------------------------------------------------------------------
-- Content Making
-------------------------------------------------------------------------------
makeUsername :: UserNumber -> T.Text
makeUsername u = ("username-" `T.append` toText u)

makeIsPrivileged :: UserNumber -> Integer
makeIsPrivileged u
    | u < 6 = 1
    | otherwise = 0

makeObjectId :: UserNumber -> T.Text
makeObjectId u = ("objectid-" `T.append` toText u)

makePuid :: UserNumber -> T.Text
makePuid u = ("puid-" `T.append` toText u)

makeUpn :: UserNumber -> T.Text
makeUpn u = ("upn-" `T.append` toText u)

makeTimeStamp :: ActivityNumber -> T.Text
makeTimeStamp a = desiredTimeStamp

-------------------------------------------------------------------------------
-- Activity Json Making
-------------------------------------------------------------------------------
headerActivity :: ActivityParameters -> JsonHeader
headerActivity (t, at, u, a) = makeHeader esIndexActivity (esTypeActivity at) (esIdActivity t u a)

deleteActivity :: ActivityParameters -> JsonDelete
deleteActivity (t, at, u, a) = makeDelete esIndexActivity (esTypeActivity at) (esIdActivity t u a)

contentActivity :: ActivityParameters -> JsonContentActivity
contentActivity (t, Abnormal, u, a) = ActivityAbnormal   
    { isPrivileged  = makeIsPrivileged u
    , objectId      = makeObjectId u
    , puid          = makePuid u
    , userName      = makeUsername u
    , tenantId              = t
    , timeStamp             = makeTimeStamp a
    , upn                   = makeUpn u
    , userInfoResolveState  = 1
    -- , previousIpAddress             = "previousIpAddress-" `T.append` (toText a)
    -- , previousCityStateCountry      = "previousCityStateCountry-" `T.append` (toText a)
    -- , callerIp                      = "callerIp-" `T.append` (toText a)
    -- , location                      = "location-" `T.append` (toText a)
    -- , previousLocationTimeStamp     = makeTimeStamp a
    } 
contentActivity (t, Blacklist, u, a) = ActivityBlacklist
    { isPrivileged  = makeIsPrivileged u
    , objectId      = makeObjectId u
    , puid          = makePuid u
    , userName      = makeUsername u
    , tenantId              = t
    , timeStamp             = makeTimeStamp a
    , upn                   = makeUpn u
    , userInfoResolveState  = 1
    -- , city                          = "city-" `T.append` (toText a)
    -- , cityStateCountry              = "cityStateCountry-" `T.append` (toText a)
    -- , country                       = "country-" `T.append` (toText a)
    -- , customerUseFullNessFeedback   = 0
    -- , insertionTime                 = makeTimeStamp a
    -- , ipAddress                     = "ipAddress-" `T.append` (toText a)
    -- , loginCount                    = 2
    -- , userDomain                    = "userDomain-" `T.append` (toText a)
    -- , userInfoResolveTime           = makeTimeStamp a
    } 
contentActivity (t, Familiar, u, a) = ActivityFamiliar
    { isPrivileged  = makeIsPrivileged u
    , objectId      = makeObjectId u
    , puid          = makePuid u
    , userName      = makeUsername u
    , tenantId              = t
    , timeStamp             = makeTimeStamp a
    , upn                   = makeUpn u
    , userInfoResolveState  = 1
    } 
contentActivity (t, Infected, u, a) = ActivityInfected
    { isPrivileged  = makeIsPrivileged u
    , objectId      = makeObjectId u
    , puid          = makePuid u
    , userName      = makeUsername u
    , tenantId              = t
    , timeStamp             = makeTimeStamp a
    , upn                   = makeUpn u
    , userInfoResolveState  = 1
    } 
contentActivity (t, Leaked, u, a) = ActivityLeaked
    { isPrivileged  = makeIsPrivileged u
    , objectId      = makeObjectId u
    , puid          = makePuid u
    , userName      = makeUsername u
    , tenantId              = t
    , timeStamp             = makeTimeStamp a
    , upn                   = makeUpn u
    , userInfoResolveState  = 1
    } 
contentActivity (t, Suspicious, u, a) = ActivitySuspicious
    { isPrivileged  = makeIsPrivileged u
    , objectId      = makeObjectId u
    , puid          = makePuid u
    , userName      = makeUsername u
    , tenantId              = t
    , timeStamp             = makeTimeStamp a
    , upn                   = makeUpn u
    , userInfoResolveState  = 1
    -- , ipAddress                     = "ipAddress-" `T.append` (toText a)
    -- , cityStateCountry              = "cityStateCountry-" `T.append` (toText a)
    -- , loginCount                    = a
    -- , insertionTime                 = makeTimeStamp a
    } 

-------------------------------------------------------------------------------
-- Riskscore Json Making
-------------------------------------------------------------------------------

-------------------------------------------------------------------------------
-- COMBINE THE STRINGS
-------------------------------------------------------------------------------
combineByteStrings :: [B.ByteString] -> B.ByteString 
combineByteStrings = concatList . (List.intersperse "\n")
    where concatList = foldl1 B.append

-------------------------------------------------------------------------------
-- Apply parameters
-- InputActivityParameters to ActivityParameters to JsonHeader   to B.ByteString
--                                               to JsonContentActivity to B.ByteString
-------------------------------------------------------------------------------

-- Creates json lines (header + content) for a single activity
addActivityString :: ActivityParameters -> B.ByteString
addActivityString ap = combineByteStrings $ [encode (headerActivity ap), encode (contentActivity ap)]

addActivityStrings :: InputActivityParameters -> B.ByteString
addActivityStrings i = combineByteStrings $ map addActivityString (generateActivityParameters i)

deleteActivityString :: ActivityParameters -> B.ByteString
deleteActivityString = encode . deleteActivity

deleteActivityStrings :: InputActivityParameters -> B.ByteString
deleteActivityStrings i = combineByteStrings $ map deleteActivityString (generateActivityParameters i)

-- Creates a list of activity parameters, one per JSON record
generateActivityParameters :: InputActivityParameters -> [ActivityParameters]
generateActivityParameters (t, x, y) = [(t, at, u, a) | at <- ats, u <- uts, a <- as]
    where ats = [Abnormal ..]
          uts = [1 .. x]
          as  = [1 .. y]

-------------------------------------------------------------------------------
-- Riskscore Json Making
-------------------------------------------------------------------------------
--
headerRiskscore :: RiskscoreParameters -> JsonHeader
headerRiskscore rp = makeHeader esIndexRiskscore esTypeRiskscore (esIdRiskscore rp)

deleteRiskscore :: RiskscoreParameters -> JsonDelete
deleteRiskscore rp = makeDelete esIndexRiskscore esTypeRiskscore (esIdRiskscore rp)

makeScoreResult :: UserNumber -> Integer
makeScoreResult u
    | u <= 5 = 1000
    | u <= 10 = 800
    | u <= 15 = 600
    | u <= 20 = 500
    | u <= 25 = 400
    | otherwise = 200

contentRiskscore :: RiskscoreParameters -> JsonContentRiskscore
contentRiskscore (t, u) = JsonContentRiskscore   
    { risk_objectId             = makeObjectId u
    , risk_latestEventTimeInUtc = desiredTimeStamp
    , risk_scoreResult          = makeScoreResult u
    , risk_tenantId             = t
    , risk_userInfoResolveState = 1
    , risk_userName             = makeUsername u
    -- , risk_algorithmVersion      = 1
    -- , risk_msodsTenantRegion     = "US"
    -- , risk_scoreNumericValue     = 100
    -- , risk_scoreReason           = "3"
    -- , risk_scoreTimeInUtc        = desiredTimeStamp
    } 

data JsonContentRiskscore = JsonContentRiskscore
        { risk_objectId             :: !T.Text
        , risk_latestEventTimeInUtc :: !T.Text
        , risk_scoreResult          :: !Integer
        , risk_tenantId             :: !T.Text
        , risk_userInfoResolveState :: !Integer
        , risk_userName             :: !T.Text
        -- , risk_algorithmVersion      :: !Integer 
        -- , risk_msodsTenantRegion     :: !T.Text
        -- , risk_scoreNumericValue     :: !Integer 
        -- , risk_scoreReason           :: !T.Text
        -- , risk_scoreTimeInUtc        :: !T.Text
        } 
    deriving (Show,Generic)

-- Capitalize first letter as record names must start with lowercase
instance ToJSON JsonContentRiskscore 
    where toJSON = genericToJSON defaultOptions { fieldLabelModifier = capitalizeFirst . (drop 5) }

-------------------------------------------------------------------------------
-- Apply parameters
-- InputRiskscoreParameters to RiskscoreParameters to JsonHeader   to B.ByteString
--                                               to JsonContentRiskscore to B.ByteString
-------------------------------------------------------------------------------
type InputRiskscoreParameters = (TenantId, UserCount)
type RiskscoreParameters = (TenantId, UserNumber)

addRiskscoreString :: RiskscoreParameters -> B.ByteString
addRiskscoreString rp = combineByteStrings $ [encode (headerRiskscore rp), encode (contentRiskscore rp)]

addRiskscoreStrings :: InputRiskscoreParameters -> B.ByteString
addRiskscoreStrings i = combineByteStrings $ map addRiskscoreString (generateRiskscoreParameters i)

deleteRiskscoreString :: RiskscoreParameters -> B.ByteString
deleteRiskscoreString = encode . deleteRiskscore

deleteRiskscoreStrings :: InputRiskscoreParameters -> B.ByteString
deleteRiskscoreStrings i = combineByteStrings $ map deleteRiskscoreString (generateRiskscoreParameters i)

-- Creates a list of riskscore parameters, one per JSON record
generateRiskscoreParameters :: InputRiskscoreParameters -> [RiskscoreParameters]
generateRiskscoreParameters (t, x) = [(t, u) | u <- uts]
    where uts = [1 .. x]
-------------------------------------------------------------------------------
-- Endpoint-related
-------------------------------------------------------------------------------
esIndexActivity :: EsIndex
esIndexActivity = "processedevents_v3.2015_12"

esTypeActivity :: ActivityType -> EsActivityType
esTypeActivity Abnormal   = "abnormallogin"
esTypeActivity Blacklist  = "azblacklistlogin"
esTypeActivity Familiar   = "familiarfeatureevent"
esTypeActivity Infected   = "infecteddevicelogin"
esTypeActivity Leaked     = "compromisedcredentials"
esTypeActivity Suspicious = "azdsuspiciouslogin"

esIdActivity :: TenantId -> UserNumber -> ActivityNumber -> EsId 
esIdActivity t u a = t `T.append` "-" `T.append` (makeObjectId u) `T.append` "-activityinstance-" `T.append` (toText a)

esIndexRiskscore :: EsIndex
esIndexRiskscore = "riskscoreevents"

esTypeRiskscore :: EsActivityType
esTypeRiskscore = "riskscorecurrentrecord"

esIdRiskscore :: RiskscoreParameters -> EsId
esIdRiskscore (t, u) = t `T.append` "-" `T.append` (makeObjectId u)
-------------------------------------------------------------------------------
-- Let's go!
-------------------------------------------------------------------------------
type UserCount = Integer
type ActivityPerUserCount = Integer
type InputActivityParameters = (TenantId,  UserCount, ActivityPerUserCount)

filenameActivityAdd :: FilePath
filenameActivityAdd = "activities-add.json"
filenameActivityDelete :: FilePath
filenameActivityDelete = "activities-delete.json"
filenameRiskscoreAdd :: FilePath
filenameRiskscoreAdd = "riskscore-add.json"
filenameRiskscoreDelete :: FilePath
filenameRiskscoreDelete = "riskscore-delete.json"

desiredTimeStamp :: T.Text
desiredTimeStamp = "2016-02-28T00:00:00"

generateQueries :: InputActivityParameters -> IO()
generateQueries (t, x, y) = do
    B.writeFile filenameActivityAdd $ addActivityStrings (t, x, y)
    B.writeFile filenameActivityDelete $ deleteActivityStrings (t, x, y)
    B.writeFile filenameRiskscoreAdd $ addRiskscoreStrings (t, x)
    B.writeFile filenameRiskscoreDelete $ deleteRiskscoreStrings (t, x)

testit :: IO ()
testit = generateQueries (desiredTenantId, desiredUserCount, desiredNumActivityPerUser)
    where desiredUserCount = 84
          desiredNumActivityPerUser = 3
          desiredTenantId = "f86dda3c-3025-4d98-b7ed-681da951d113"
          -- desiredTenantId = "8b6f70c0-65f8-4492-80c0-36520b9fd5b3"

-- Main
main :: IO ()
main = putStrLn "Hello World"
