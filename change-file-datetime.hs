import Data.Bifunctor (Bifunctor (first))
import Data.Function ((&))
import Data.Time
import System.Environment (getArgs)
import System.Process (callCommand, readProcess)

data DateType = ModifiedDate | CreatedDate deriving (Show, Read, Eq)

main :: IO ()
main = do
  args <- getArgs
  let (offset, dateType, files) = extractArgs args
  creationdates <- lines <$> readProcess "stat" (buildReadProcessArgs dateType files) ""
  commands <- mapM (buildUpdateCommand dateType . first (fmap (formatDate dateType) . addOffsetToDate offset)) $ zip creationdates files
  mapM_ callCommand commands
  putStrLn $ successMessage dateType

extractArgs :: [String] -> (NominalDiffTime, DateType, [String])
extractArgs ("-c" : offset : files) = (toTimeDiff offset, CreatedDate, files)
extractArgs (offset : "-c" : files) = (toTimeDiff offset, CreatedDate, files)
extractArgs ("-m" : offset : files) = (toTimeDiff offset, ModifiedDate, files)
extractArgs (offset : "-m" : files) = (toTimeDiff offset, ModifiedDate, files)
extractArgs (offset : files) = (toTimeDiff offset, ModifiedDate, files)

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
