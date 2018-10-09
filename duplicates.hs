#!/usr/bin/env stack
-- stack --resolver lts-12.5 script

{-# LANGUAGE OverloadedStrings #-}

import Prelude hiding (lines, FilePath)
import Turtle hiding (sort)

import Data.List (sort, group)
import Data.Text (breakOn, lines)

main :: IO ()
main = do
  inFile "database" "memrise_database.csv"
  inFile "levels"   "memrise_levels.csv"

inFile :: String -> FilePath -> IO ()
inFile shortName fileName = do
  file <- readTextFile fileName
  putStrLn $ "Duplicates in " ++ shortName ++ " file …"
  mapM_ (echo . head) $ detectDups file

detectDups :: Text -> [[Line]]
detectDups = notUnique . group . sort . polish . lines
  where
  notUnique = filter ((>1) . length)
  polish    = map (unsafeTextToLine . fst . breakOn ",")
