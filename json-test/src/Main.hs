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

makeHeader :: EsIndex -> EsActivityType -> EsId -> Header
makeHeader _index _type _id = Header $ HeaderInner _index _type _id

makeEsId :: TenantId -> UserNumber -> ActivityNumber -> EsId 
makeEsId t u a = t `T.append` "-u-" `T.append` (toText u) `T.append` "-a-" `T.append` (toText a)

activityHeader :: ActivityParameters -> Header
activityHeader (t, at, u, a) = makeHeader esIndexActivities (getEsType at) (makeEsId t u a)

-------------------------------------------------------------------------------
-- Content Making
-------------------------------------------------------------------------------
makeUsername :: UserNumber -> T.Text
makeUsername u = ("username-" `T.append` toText u)

activityContent :: ActivityParameters -> ActivityData
activityContent (t, Abnormal  , u, a) = ActivityAbnormal   t (toText u) (makeUsername u) (toText u) True
activityContent (t, Blacklist , u, a) = ActivityBlacklist  t (toText u) (makeUsername u) (toText u) True
activityContent (t, Suspicious, u, a) = ActivitySuspicious t (toText u) (makeUsername u) (toText u) True
activityContent (t, Leaked    , u, a) = ActivityLeaked     t (toText u) (makeUsername u) (toText u) True
activityContent (t, Familiar  , u, a) = ActivityFamiliar   t (toText u) (makeUsername u) (toText u) True
activityContent (t, Infected  , u, a) = ActivityInfected   t (toText u) (makeUsername u) (toText u) True

-------------------------------------------------------------------------------
-- Apply parameters
-------------------------------------------------------------------------------
combineByteStrings :: [B.ByteString] -> B.ByteString 
combineByteStrings = concatList . (List.intersperse "\n")
    where concatList = foldl1 B.append

activityString :: ActivityParameters -> B.ByteString
activityString ap = combineByteStrings $ [encode (activityHeader ap), encode (activityContent ap)]

activityStrings :: InputParameters -> B.ByteString
activityStrings i = combineByteStrings $ map activityString parameters
    where parameters = parametersList i :: [ActivityParameters]

-- Creates a list of activity parameters, one per JSON record
parametersList :: InputParameters -> [ActivityParameters]
parametersList (t, x, y) = [(t, at, u, a) | at <- ats, u <- uts, a <- as]
    where ats = [Abnormal ..]
          uts = [0 .. x]
          as = [0 .. y]


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

esIndexRiskscore :: EsIndex
esIndexRiskscore = "riskscoreevents"

-------------------------------------------------------------------------------
-- Let's go!
-------------------------------------------------------------------------------
type UserCount = Integer
type ActivitiesPerUserCount = Integer
type InputParameters = (TenantId,  UserCount, ActivitiesPerUserCount)

generateActivities :: InputParameters -> IO()
generateActivities i = B.writeFile filename $ activityStrings i
    where filename = "temp.json"

testit :: IO ()
testit = generateActivities (desiredTenantId, desiredUserCount, desiredNumActivitiesPerUser)
    where desiredUserCount = 4
          desiredNumActivitiesPerUser = 6
          desiredTenantId = "tenantIddummy"

-- Main
main :: IO ()
main = putStrLn "Hello World"
