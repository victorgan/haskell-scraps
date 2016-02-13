{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}
import Data.Aeson
import Data.Aeson.Types
import Data.List as List
import qualified Data.Text as T
import qualified Data.ByteString.Lazy as B
-- import Network.HTTP.Conduit (simpleHttp)
import GHC.Generics

-------------------------------------------------------------------------------
-- Data
data Header = Header 
    { create :: !HeaderContent 
    } deriving (Show,Generic)
instance ToJSON Header where toJSON = genericToJSON defaultOptions

data HeaderContent = HeaderContent 
    { _index :: !EsIndex
    , _type  :: !EsActivityType
    , _id    :: !EsId
    } deriving (Show,Generic)
instance ToJSON HeaderContent where toJSON = genericToJSON defaultOptions

data ActivityAbnormal = ActivityAbnormal 
    { abnormalTenantId :: !T.Text
    , abnormalObjectId :: !T.Text
    , abnormalUserName :: !T.Text
    , abnormalPuid     :: !T.Text
    , abnormalCallerIp :: !Bool
    } deriving (Show,Generic)
instance ToJSON ActivityAbnormal 
    where toJSON = genericToJSON defaultOptions { fieldLabelModifier = drop 8}

data ActivityBlacklist = ActivityBlacklist 
    { blacklistTenantId :: !T.Text
    , blacklistObjectId :: !T.Text
    , blacklistUserName :: !T.Text
    , blacklistPuid     :: !T.Text
    , blacklistCallerIp :: !Bool
    } deriving (Show,Generic)
instance ToJSON ActivityBlacklist 
    where toJSON = genericToJSON defaultOptions { fieldLabelModifier = drop 9 }

data ActivityType = Abnormal | Blacklist | Suspicious | Leaked | Familiar | Infected
-------------------------------------------------------------------------------
-- Types
type TenantId = T.Text
type UserNumber = Integer
type ActivityNumber = Integer
type EsId = T.Text
type EsIndex = T.Text
type EsActivityType = T.Text

-------------------------------------------------------------------------------
-- Header Making
makeHeader :: EsIndex -> EsActivityType -> EsId -> Header
makeHeader _index _type _id = Header $ HeaderContent _index _type _id

makeHeaderId :: TenantId -> UserNumber -> ActivityNumber -> EsId 
makeHeaderId t u a = t `T.append` "-u-" `T.append` (toText u) `T.append` "-a-" `T.append` (toText a)

headerActivity :: ActivityType -> (TenantId -> UserNumber -> ActivityNumber -> Header)
headerActivity at t u a = makeHeader esIndexActivities (getEsType at) (makeHeaderId t u a)

headerAbnormal :: TenantId -> UserNumber -> ActivityNumber -> Header
headerAbnormal = headerActivity Abnormal

headerBlacklist :: TenantId -> UserNumber -> ActivityNumber -> Header
headerBlacklist = headerActivity Blacklist

-------------------------------------------------------------------------------
-- Content Making
toText :: UserNumber -> T.Text
toText = T.pack . show

contentAbnormal :: TenantId -> UserNumber -> ActivityNumber -> ActivityAbnormal
contentAbnormal t u a = ActivityAbnormal t (toText u) (toText a) (toText u) True

contentBlacklist :: TenantId -> UserNumber -> ActivityNumber -> ActivityBlacklist
contentBlacklist t u a = ActivityBlacklist t (toText u) (toText a) (toText u) True

-------------------------------------------------------------------------------
-- Apply parameters
encodeCommand :: (ToJSON a, ToJSON b) => a -> b -> [B.ByteString]
encodeCommand a b = [encode a, encode b];

encodeContent :: ActivityType -> TenantId -> UserNumber -> ActivityNumber -> B.ByteString
encodeContent _ t u a = encode $ contentAbnormal t u a

combineByteStrings :: [B.ByteString] -> B.ByteString 
combineByteStrings = concatList . (List.intersperse "\n")
    where concatList = foldl1 B.append

commandAbnormal :: UserNumber -> ActivityNumber -> B.ByteString
commandAbnormal u a = combineByteStrings $ encodeCommand (headerAbnormal t u a) (contentAbnormal t u a)
    where t = desiredTenantId :: TenantId

commandAbnormala :: UserNumber -> [ActivityNumber] -> B.ByteString
commandAbnormala u as = combineByteStrings $ map (commandAbnormal u) as

commandAbnormalua :: [UserNumber] -> [ActivityNumber] -> B.ByteString
commandAbnormalua us as = combineByteStrings $ map (flip commandAbnormala as) us



commandActivity :: ActivityType -> UserNumber -> ActivityNumber -> B.ByteString
commandActivity at u a = jojo (contentAbnormal t u a)
    where jojo content = combineByteStrings $ encodeCommand (headerActivity at t u a) content
          t = desiredTenantId :: TenantId

commandActivitya :: ActivityType -> UserNumber -> [ActivityNumber] -> B.ByteString
commandActivitya at u as = combineByteStrings $ map (commandActivity at u) as

commandActivityua :: ActivityType -> [UserNumber] -> [ActivityNumber] -> B.ByteString
commandActivityua at us as = combineByteStrings $ map (flip (commandActivitya at) as) us




commandBlacklist :: UserNumber -> ActivityNumber -> B.ByteString
commandBlacklist u a = combineByteStrings $ encodeCommand (headerBlacklist t u a) (contentBlacklist t u a)
    where t = desiredTenantId :: TenantId

commandBlacklista :: UserNumber -> B.ByteString
commandBlacklista u = combineByteStrings $ map (commandBlacklist u) as
    where as = activityRange :: [ActivityNumber]

commandBlacklistua :: B.ByteString
commandBlacklistua = combineByteStrings $ map commandBlacklista us
    where us = userRange :: [UserNumber]

commandsActivities :: B.ByteString
commandsActivities = combineByteStrings [(commandAbnormalua userRange activityRange), commandBlacklistua]

-------------------------------------------------------------------------------
-- Endpoint-related

esIndexActivities :: EsIndex
esIndexActivities = "processedevents_v3.2015_12"

getEsType :: ActivityType -> EsActivityType
getEsType Abnormal   = "abnormallogin"
getEsType Blacklist  = "azblacklistlogin"
getEsType Suspicious = "azdsuspiciouslogin"
getEsType Leaked     = "compromisedcredentials"
getEsType Familiar   = "familiarfeatureevent"
getEsType Infected   = "infecteddevicelogin"


-------------------------------------------------------------------------------
-- Input
filename :: FilePath
filename = "temp.json"

desiredTenantId :: T.Text
desiredTenantId = "tenantIddummy"

type UserCount = Integer
type ActivityCountPerUser = Integer
desiredUserCount :: UserCount
desiredUserCount = 2

userRange :: [UserNumber]
userRange = [0 .. u] :: [UserNumber]
    where u = desiredUserCount

desiredNumActivitiesPerUser :: ActivityCountPerUser
desiredNumActivitiesPerUser = 3

activityRange :: [ActivityCountPerUser]
activityRange = [1 .. desiredNumActivitiesPerUser]


-------------------------------------------------------------------------------
generateActivities :: UserCount -> ActivityCountPerUser -> IO()
generateActivities _ _ = B.writeFile filename commandsActivities

testit :: IO ()
testit = generateActivities desiredUserCount desiredNumActivitiesPerUser

-- Random tests

-- Transform





-- Main
main :: IO ()
main = putStrLn "Hello World"
