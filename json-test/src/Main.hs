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
-------------------------------------------------------------------------------
data Header = Header 
    { create :: !HeaderInner 
    } deriving (Show,Generic)
instance ToJSON Header where toJSON = genericToJSON defaultOptions

data HeaderInner = HeaderInner 
    { _index :: !EsIndex
    , _type  :: !EsActivityType
    , _id    :: !EsId
    } deriving (Show,Generic)
instance ToJSON HeaderInner where toJSON = genericToJSON defaultOptions

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
                  | ActivityFamiliar
                        { tenantId :: !T.Text
                        , objectId :: !T.Text
                        , userName :: !T.Text
                        , puid     :: !T.Text
                        , callerIp :: !Bool
                        } 
                  | ActivityInfected 
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
-- Type Synonyms
-------------------------------------------------------------------------------
type TenantId = T.Text
type UserNumber = Integer
type ActivityNumber = Integer
type EsId = T.Text
type EsIndex = T.Text
type EsActivityType = T.Text

-------------------------------------------------------------------------------
-- Header Making
-------------------------------------------------------------------------------
toText :: Integer -> T.Text
toText = T.pack . show

makeHeader :: EsIndex -> EsActivityType -> EsId -> Header
makeHeader _index _type _id = Header $ HeaderInner _index _type _id

makeEsId :: TenantId -> UserNumber -> ActivityNumber -> EsId 
makeEsId t u a = t `T.append` "-u-" `T.append` (toText u) `T.append` "-a-" `T.append` (toText a)

headerActivity :: ActivityType -> TenantId -> UserNumber -> ActivityNumber -> Header
headerActivity at t u a = makeHeader esIndexActivities (getEsType at) (makeEsId t u a)

-------------------------------------------------------------------------------
-- Content Making
-------------------------------------------------------------------------------
makeUsername :: UserNumber -> T.Text
makeUsername u = ("username-" `T.append` toText u)

contentActivity :: ActivityType -> TenantId -> UserNumber -> ActivityNumber -> ActivityData
contentActivity Abnormal t u a   = ActivityAbnormal t (toText u) (makeUsername u) (toText u) True
contentActivity Blacklist t u a  = ActivityBlacklist t (toText u) (makeUsername u) (toText u) True
contentActivity Suspicious t u a = ActivitySuspicious t (toText u) (makeUsername u) (toText u) True
contentActivity Leaked t u a     = ActivityLeaked t (toText u) (makeUsername u) (toText u) True
contentActivity Familiar t u a   = ActivityFamiliar t (toText u) (makeUsername u) (toText u) True
contentActivity Infected t u a   = ActivityInfected t (toText u) (makeUsername u) (toText u) True

-------------------------------------------------------------------------------
-- Apply parameters
-------------------------------------------------------------------------------
combineByteStrings :: [B.ByteString] -> B.ByteString 
combineByteStrings = concatList . (List.intersperse "\n")
    where concatList = foldl1 B.append

-- creates a single activity bytestring
commandActivity :: ActivityType -> UserNumber -> ActivityNumber -> B.ByteString
commandActivity at u a = combineByteStrings $ encodeCommand (headerActivity at t u a) (contentActivity at t u a)
    where t = desiredTenantId :: TenantId
          encodeCommand :: (ToJSON a, ToJSON b) => a -> b -> [B.ByteString]
          encodeCommand x y = [encode x, encode y]

-- creates a bytestring with activities for a list of activity numbers
commandActivitya :: ActivityType -> UserNumber -> [ActivityNumber] -> B.ByteString
commandActivitya at u as = combineByteStrings $ map (commandActivity at u) as

-- for list of activity numbers and user numbers
commandActivityua :: ActivityType -> [UserNumber] -> [ActivityNumber] -> B.ByteString
commandActivityua at us as = combineByteStrings $ map (flip (commandActivitya at) as) us

-- for list of activity numbers user numbers, and activity types
commandActivityatua :: [ActivityType] -> [UserNumber] -> [ActivityNumber] -> B.ByteString
commandActivityatua ats us as = combineByteStrings $ map commandActivityua' ats
    where commandActivityua' at = commandActivityua at us as

-- creates bytestrings for y activites per each x user per each activity type
type UserCount = Integer
type ActivityCountPerUser = Integer
commandsActivities :: UserCount -> ActivityCountPerUser -> B.ByteString
commandsActivities x y = commandActivityatua [Abnormal .. ] userRange activityRange
    where userRange = [0 .. x] :: [UserNumber]
          activityRange = [1 .. y] :: [ActivityCountPerUser]

-------------------------------------------------------------------------------
-- Endpoint-related
-------------------------------------------------------------------------------
esIndexActivities :: EsIndex
esIndexActivities = "processedevents_v3.2015_12"

getEsType :: ActivityType -> EsActivityType
getEsType Abnormal   = "abnormallogin"
getEsType Blacklist  = "azblacklistlogin"
getEsType Suspicious = "azdsuspiciouslogin"
getEsType Leaked     = "compromisedcredentials"
getEsType Familiar   = "familiarfeatureevent"
getEsType Infected   = "infecteddevicelogin"

desiredTenantId :: T.Text
desiredTenantId = "tenantIddummy"

esIndexRiskscore :: EsIndex
esIndexRiskscore = "riskscoreevents"

-------------------------------------------------------------------------------
-- Let's go!
-------------------------------------------------------------------------------
generateActivities :: FilePath -> UserCount -> ActivityCountPerUser -> IO()
generateActivities filename x y = B.writeFile filename $ commandsActivities x y

testit :: IO ()
testit = generateActivities filename desiredUserCount desiredNumActivitiesPerUser
    where desiredUserCount = 4
          desiredNumActivitiesPerUser = 6
          filename = "temp.json"

-- Main
main :: IO ()
main = putStrLn "Hello World"
