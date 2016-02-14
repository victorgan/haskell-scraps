{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}
import Data.Aeson
import Data.Aeson.Types
import Data.List as List
import Data.Char as Char
import qualified Data.Text as T
import qualified Data.ByteString.Lazy as B
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

data ActivityData = ActivityAbnormal 
                        { tenantId :: !T.Text
                        , objectId :: !T.Text
                        , userName :: !T.Text
                        , puid     :: !T.Text
                        , callerIp :: !Bool
                        } 
                  | ActivityBlacklist 
                        { tenantId :: !T.Text
                        , objectId :: !T.Text
                        , userName :: !T.Text
                        , puid     :: !T.Text
                        , callerIp :: !Bool
                        } 
                  | ActivitySuspicious 
                        { tenantId :: !T.Text
                        , objectId :: !T.Text
                        , userName :: !T.Text
                        , puid     :: !T.Text
                        , callerIp :: !Bool
                        } 
                  | ActivityLeaked 
                        { tenantId :: !T.Text
                        , objectId :: !T.Text
                        , userName :: !T.Text
                        , puid     :: !T.Text
                        , callerIp :: !Bool
                        } 
                    deriving (Show,Generic)

-- Hack: hide tag for sum datatype by setting its field to TenantId, which is
-- then overwritten. Also capitalize first letter as record names must start
-- with lowercase.
instance ToJSON ActivityData 
    where toJSON = genericToJSON defaultOptions { fieldLabelModifier = capitalizeFirst
                                                , sumEncoding = TaggedObject 
                                                    { tagFieldName      = "TenantId"
                                                    , contentsFieldName = "contents"
                                                    }
                                                }
capitalizeFirst :: String -> String
capitalizeFirst (x:xs) = (Char.toUpper x):xs
capitalizeFirst [] = []

data ActivityType = Abnormal | Blacklist | Suspicious | Leaked | Familiar | Infected
    deriving Enum

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

-------------------------------------------------------------------------------
-- Content Making
toText :: UserNumber -> T.Text
toText = T.pack . show

contentData :: ActivityType -> TenantId -> UserNumber -> ActivityNumber -> ActivityData
contentData Abnormal t u a   = ActivityAbnormal t (toText u) ("username-" `T.append` toText u) (toText u) True
contentData Blacklist t u a  = ActivityBlacklist t (toText u) ("username-" `T.append` toText u) (toText u) True
contentData Suspicious t u a = ActivitySuspicious t (toText u) ("username-" `T.append` toText u) (toText u) True
contentData Leaked t u a     = ActivityLeaked t (toText u) ("username-" `T.append` toText u) (toText u) True
contentData _ t u a          = ActivityLeaked t (toText u) ("username-" `T.append` toText u) (toText u) True

-------------------------------------------------------------------------------
-- Apply parameters
encodeCommand :: (ToJSON a, ToJSON b) => a -> b -> [B.ByteString]
encodeCommand a b = [encode a, encode b];

encodeContent :: ActivityType -> TenantId -> UserNumber -> ActivityNumber -> B.ByteString
encodeContent at t u a = encode $ contentData at t u a

encodeHeader ::  ActivityType -> TenantId -> UserNumber -> ActivityNumber -> B.ByteString
encodeHeader at t u a = encode $ headerActivity at t u a

combineByteStrings :: [B.ByteString] -> B.ByteString 
combineByteStrings = concatList . (List.intersperse "\n")
    where concatList = foldl1 B.append

-- creates a single activity bytestring
commandActivity :: ActivityType -> UserNumber -> ActivityNumber -> B.ByteString
commandActivity at u a = combineByteStrings [(encodeHeader at t u a), (encodeContent at t u a)]
    where t = desiredTenantId :: TenantId

commandActivitya :: ActivityType -> UserNumber -> [ActivityNumber] -> B.ByteString
commandActivitya at u as = combineByteStrings $ map (commandActivity at u) as

commandActivityua :: ActivityType -> [UserNumber] -> [ActivityNumber] -> B.ByteString
commandActivityua at us as = combineByteStrings $ map (flip (commandActivitya at) as) us

-- creates bytestrings for each activity types, x activties and y users
commandActivityatua :: [ActivityType] -> [UserNumber] -> [ActivityNumber] -> B.ByteString
commandActivityatua ats us as = combineByteStrings $ map commandActivityua' ats
    where commandActivityua' at = commandActivityua at us as

commandsActivities :: UserCount -> ActivityCountPerUser -> B.ByteString
commandsActivities x y = commandActivityatua [Abnormal .. ] userRange activityRange
    where userRange = [0 .. x] :: [UserNumber]
          activityRange = [1 .. y] :: [ActivityCountPerUser]

-------------------------------------------------------------------------------
-- Endpoint-related
esIndexActivities :: EsIndex
esIndexActivities = "processedevents_v3.2015_12"

esIndexRiskscore :: EsIndex
esIndexRiskscore = "riskscoreevents"

getEsType :: ActivityType -> EsActivityType
getEsType Abnormal   = "abnormallogin"
getEsType Blacklist  = "azblacklistlogin"
getEsType Suspicious = "azdsuspiciouslogin"
getEsType Leaked     = "compromisedcredentials"
getEsType Familiar   = "familiarfeatureevent"
getEsType Infected   = "infecteddevicelogin"

desiredTenantId :: T.Text
desiredTenantId = "tenantIddummy"

type UserCount = Integer
type ActivityCountPerUser = Integer

filename :: FilePath
filename = "temp.json"

desiredUserCount :: UserCount
desiredUserCount = 2

desiredNumActivitiesPerUser :: ActivityCountPerUser
desiredNumActivitiesPerUser = 3

-------------------------------------------------------------------------------
-- Let's go!
generateActivities :: UserCount -> ActivityCountPerUser -> IO()
generateActivities x y = B.writeFile filename $ commandsActivities x y

testit :: IO ()
testit = generateActivities desiredUserCount desiredNumActivitiesPerUser

-- Main
main :: IO ()
main = putStrLn "Hello World"
