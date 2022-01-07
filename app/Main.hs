module Main where

import Data.Bifunctor (Bifunctor (first))
import Data.Function ((&))
import Data.Time
import System.Environment (getArgs)
import System.Process (callCommand, readProcess)

data DateType = ModifiedDate | CreatedDate deriving (Show, Read, Eq)

main :: IO ()
main = do
  (offset, dateType, files) <- parseArgs <$> getArgs 
  creationdates <- lines <$> readProcess "stat" (buildReadProcessArgs dateType files) ""
  commands <- mapM (buildUpdateCommand dateType . first (fmap (formatDate dateType) . addOffsetToDate offset)) $ zip creationdates files
  mapM_ callCommand commands
  putStrLn $ successMessage dateType

parseArgs :: [String] -> (NominalDiffTime, DateType, [String])
parseArgs ("-c" : offset : files) = (toTimeDiff offset, CreatedDate, files)
parseArgs (offset : "-c" : files) = (toTimeDiff offset, CreatedDate, files)
parseArgs ("-m" : offset : files) = (toTimeDiff offset, ModifiedDate, files)
parseArgs (offset : "-m" : files) = (toTimeDiff offset, ModifiedDate, files)
parseArgs (offset : files) = (toTimeDiff offset, ModifiedDate, files)
parseArgs [] = (toTimeDiff "0", ModifiedDate, [""])

toTimeDiff :: String -> NominalDiffTime
toTimeDiff = realToFrac . read

successMessage :: DateType -> String
successMessage ModifiedDate = "Changed modification date."
successMessage CreatedDate = "Changed creation date."

addOffsetToDate :: NominalDiffTime -> [Char] -> IO ZonedTime
addOffsetToDate offset =
  utcToLocalZonedTime
    . addUTCTime offset
    . parseUnixTime
    . removeQuotations

usFormat :: [Char]
usFormat = "%m/%d/%Y %T"

mTimeFormat :: [Char]
mTimeFormat = "%Y%m%d%H%M.%S"

formatDate :: DateType -> ZonedTime -> String
formatDate ModifiedDate = formatTime defaultTimeLocale mTimeFormat
formatDate CreatedDate = show . formatTime defaultTimeLocale usFormat

unixTimeFormat :: [Char]
unixTimeFormat = "%Y-%-m-%-d %H:%M:%S%Q %z"

parseUnixTime :: String -> UTCTime
parseUnixTime = parseTimeOrError True defaultTimeLocale unixTimeFormat

removeQuotations :: [Char] -> [Char]
removeQuotations = filter (`notElem` "\"")

buildReadProcessArgs :: DateType -> [String] -> [String]
buildReadProcessArgs CreatedDate files = "-c \"%w\"" : files
buildReadProcessArgs ModifiedDate files = "-c \"%y\"" : files

buildUpdateCommand :: DateType -> (IO String, String) -> IO String
buildUpdateCommand ModifiedDate = buildUpdateCommandIOString "touch -m -t"
buildUpdateCommand CreatedDate = buildUpdateCommandIOString "SetFile -d "

buildUpdateCommandIOString :: String -> (IO String, String) -> IO String
buildUpdateCommandIOString command (date, file) = do
  dateStr <- date
  putStrLn $ command ++ file ++ dateStr
  fmap (buildUpdateCommandString command file) date

buildUpdateCommandString :: String -> String -> String -> String
buildUpdateCommandString command file date = command ++ date ++ " " ++ file
