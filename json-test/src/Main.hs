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
type EsId = T.Text
type EsIndex = T.Text
type EsActivityType = T.Text

toText :: UserNumber -> T.Text
toText = T.pack . show

makeId :: TenantId -> UserNumber -> EsId 
makeId t i = t `T.append` "-u-" `T.append` (toText i)

-- Make 
makeHeader :: EsIndex -> EsActivityType -> EsId -> Header
makeHeader _index _type _id = Header $ HeaderContent _index _type _id

makeActivityAbnormal :: TenantId -> UserNumber -> ActivityAbnormal
makeActivityAbnormal a b = ActivityAbnormal a (toText b) (toText b) (toText b) True

makeActivityBlacklist :: TenantId -> UserNumber -> ActivityBlacklist
makeActivityBlacklist a b = ActivityBlacklist a (toText b) (toText b) (toText b) True

-- Aggregate

aggActivityNormal :: TenantId -> [Integer] -> [ActivityAbnormal]
aggActivityNormal t = map (makeActivityAbnormal t)

-- Inputs
filename :: FilePath
filename = "temp.json"

writeJson :: ToJSON a => a -> IO()
writeJson toJson = B.writeFile filename $ encode toJson



-- Inputs
esIndexActivities :: EsIndex
esIndexActivities = "processedevents_v3.2015_12"

esActivityTypes :: [EsActivityType]
esActivityTypes = ["azblacklistlogin", "abnormallogin", "azdsuspiciouslogin", "compromisedcredentials", "familiarfeatureevent", "infecteddevicelogin"]

realTenantId :: T.Text
realTenantId = "tenantIddummy"


realUserId :: Integer
realUserId = 1;


encodeMap :: ToJSON a => [a] -> [B.ByteString]
encodeMap = map encode

combineByteStrings :: [B.ByteString] -> B.ByteString 
combineByteStrings = concatList . (List.intersperse "\n")
    where concatList = foldl1 B.append

encodeBulkCommand :: ToJSON a => [a] -> B.ByteString
encodeBulkCommand = combineByteStrings . encodeMap

encodeCommand :: (ToJSON a, ToJSON b) => a -> b -> [B.ByteString]
encodeCommand a b = [encode a, encode b];






headerActivity :: [B.ByteString]
headerActivity = encodeCommand blackListHeader activityContent

activityContent :: ActivityAbnormal
activityContent = makeActivityAbnormal realTenantId realUserId

-- FINAL FORM
type NumUsers = Integer
type NumActivitiesPerUser = Integer
desiredNumUsers :: NumUsers
desiredNumUsers = 2
desiredNumActivitiesPerUser :: NumActivitiesPerUser
desiredNumActivitiesPerUser = 3



generateActivities :: NumUsers -> NumActivitiesPerUser -> IO()
generateActivities _ _ = B.writeFile filename (combineByteStrings headerActivity)

testit :: IO ()
testit = generateActivities desiredNumUsers desiredNumActivitiesPerUser

-- Random tests

-- Transform
activityHeader :: EsActivityType -> TenantId -> UserNumber -> Header
activityHeader a b c = makeHeader esIndexActivities a (makeId b c)

blackListHeader :: Header
blackListHeader = activityHeader (head esActivityTypes) realTenantId realUserId




-- Main
main :: IO ()
main = putStrLn "Hello World"
