{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}
import Data.Aeson
import Data.Aeson.Types
import Data.Char as Char
import Data.List as List
import qualified Data.Text as T
import qualified Data.ByteString.Lazy as B
-- import Network.HTTP.Conduit (simpleHttp)
import GHC.Generics

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

-- Instances to convert our type to JSON.

capitalizeFirst :: String -> String
capitalizeFirst (x:xs) = (Char.toUpper x):xs
capitalizeFirst [] = []

-- Instances to convert our type to JSON.
type TenantId = T.Text
type UserNumber = Integer
type ActivityNumber = Integer
type EsId = T.Text
type EsIndex = T.Text
type EsActivityType = T.Text

toText :: UserNumber -> T.Text
toText = T.pack . show

makeId :: TenantId -> UserNumber -> ActivityNumber -> EsId 
makeId t u a = t `T.append` "-u-" `T.append` (toText u) `T.append` "-a-" `T.append` (toText a)

-- Make 
makeHeader :: EsIndex -> EsActivityType -> EsId -> Header
makeHeader _index _type _id = Header $ HeaderContent _index _type _id


-- Aggregate

encodeBulkCommand :: ToJSON a => [a] -> B.ByteString
encodeBulkCommand = combineByteStrings . encodeMap
    where encodeMap = map encode

-------------------------------------------------------------------------------
encodeCommand :: (ToJSON a, ToJSON b) => a -> b -> [B.ByteString]
encodeCommand a b = [encode a, encode b];




-------------------------------------------------------------------------------
-- Where you can input teant id, usernumber and activitynumber
headerAbnormal :: TenantId -> UserNumber -> ActivityNumber -> Header
headerAbnormal t u a = makeHeader esIndexActivities esTypeAbnormal (makeId t u a)

headerBlacklist :: TenantId -> UserNumber -> ActivityNumber -> Header
headerBlacklist t u a = makeHeader esIndexActivities esTypeBlacklist (makeId t u a)

contentAbnormal :: TenantId -> UserNumber -> ActivityNumber -> ActivityAbnormal
contentAbnormal t u a = ActivityAbnormal t (toText u) (toText a) (toText u) True

contentBlacklist :: TenantId -> UserNumber -> ActivityNumber -> ActivityBlacklist
contentBlacklist t u a = ActivityBlacklist t (toText u) (toText a) (toText u) True


-------------------------------------------------------------------------------
-- Apply parameters

-- headerAbnormal2 ::  Header
-- headerAbnormal2 = headerAbnormal temptTenantId tempUserNumber

-- contentAbnormal2 :: ActivityAbnormal
-- contentAbnormal2 = contentAbnormal desiredTenantId tempUserNumber

-- commandActivity :: ToJSON a => Header -> a -> (UserNumber -> B.ByteString)
-- commandActivity h c u = combineByteStrings $ encodeCommand (h t u) (c t u)
--     where t = desiredTenantId :: TenantId

combineByteStrings :: [B.ByteString] -> B.ByteString 
combineByteStrings = concatList . (List.intersperse "\n")
    where concatList = foldl1 B.append

commandAbnormal :: UserNumber -> ActivityNumber -> B.ByteString
commandAbnormal u a = combineByteStrings $ encodeCommand (headerAbnormal t u a) (contentAbnormal t u a)
    where t = desiredTenantId :: TenantId

commandsAbnormal :: B.ByteString
commandsAbnormal = combineByteStrings $ map (commandAbnormal u) as
    where as = activityRange :: [ActivityNumber]
          u = 1 :: UserNumber

commandBlacklist :: UserNumber -> ActivityNumber -> B.ByteString
commandBlacklist u a = combineByteStrings $ encodeCommand (headerBlacklist t u a) (contentBlacklist t u a)
    where t = desiredTenantId :: TenantId

commandsBlacklist :: B.ByteString
commandsBlacklist = combineByteStrings $ map (commandBlacklist u) as
    where as = activityRange :: [ActivityNumber]
          u = 1 :: UserNumber


commandsActivities :: B.ByteString
commandsActivities = combineByteStrings [commandsAbnormal, commandsBlacklist]

-------------------------------------------------------------------------------
-- Parameters
tempUserNumber :: Integer
tempUserNumber = 1;

filename :: FilePath
filename = "temp.json"

-------------------------------------------------------------------------------
-- Endpoint-related
esIndexActivities :: EsIndex
esIndexActivities = "processedevents_v3.2015_12"

esTypeAbnormal :: EsActivityType
esTypeAbnormal = "abnormallogin"

esTypeBlacklist :: EsActivityType
esTypeBlacklist = "azblacklistlogin"

esActivityTypes :: [EsActivityType]
esActivityTypes = ["azblacklistlogin", "abnormallogin", "azdsuspiciouslogin", "compromisedcredentials", "familiarfeatureevent", "infecteddevicelogin"]

-------------------------------------------------------------------------------
-- Input
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
