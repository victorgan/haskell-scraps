module Main where

import qualified Data.ByteString.Lazy as BL
import qualified Data.Vector as V
-- from cassava
import Data.Csv

-- a simple type alias for data
type BaseballStats = (BL.ByteString, Int, BL.ByteString, Int)

summer (name, year, team, atBats) n = n + atBats

main :: IO ()
main = do
  csvData <- BL.readFile "batting.csv"
  let v = decode NoHeader csvData :: Either String (V.Vector BaseballStats)
  let summed = fmap (V.foldr summer 0) v
  putStrLn $ "Total atBats was: " ++ (show summed)
