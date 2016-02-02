module Main where

import qualified Data.ByteString.Lazy as BL
import qualified Data.Foldable as F
import Data.Csv.Streaming -- from cassava

-- a simple type alias for data, one row of the CSV file
type BaseballStatsRow = (BL.ByteString, Int, BL.ByteString, Int)

fourthElement :: (t, t1, t2, t3) -> t3
fourthElement (_, _, _, d) = d

summer :: (t, t1, t2, Int) -> Int -> Int
summer = (+) . fourthElement

baseballStats :: BL.ByteString -> Records BaseballStatsRow
baseballStats = decode NoHeader

summed :: BL.ByteString -> Int
summed = F.foldr summer 0 . baseballStats

main :: IO ()
main = do
  csvData <- BL.readFile "batting.csv"
  putStrLn $ "Total atBats was: " ++ (show $ summed csvData)
