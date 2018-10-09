#!/usr/bin/env stack
-- stack --resolver lts-12.5 script

{-# LANGUAGE OverloadedStrings #-}

import Prelude hiding (FilePath)
import Turtle hiding (sortBy)

import Data.Function (on)
import Data.List (sortBy, groupBy)
import Data.Ord (comparing)
import qualified Data.Text as T

main :: IO ()
main = do
  databaseFile <- readTextFile "memrise_database.csv"
  levelsFile   <- readTextFile "memrise_levels.csv"
  showDups $ detectDups databaseFile
  showDups $ detectDups levelsFile
  return ()
  where
  showDups = mapM_ (\file -> echo $ fst $ head file)

detectDups :: Text -> [[(Line, Line)]]
detectDups file = filter (\xs -> length xs > 1) grouped
  where
  toPairs = map (\(a, b) -> (unsafeTextToLine a, unsafeTextToLine $ T.drop 1 b)) . map (T.breakOn ",")
  lines'  = toPairs $ T.lines file
  grouped = groupBy ((==) `on` fst) $ sortBy (comparing fst) lines'
