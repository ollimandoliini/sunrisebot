{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main (main) where

import Control.Monad (void)
import Data.Aeson (FromJSON, ToJSON)
import Data.Foldable (for_)
import Data.Text (Text, pack, unlines)
import Data.Time (
  Day,
  MonthOfYear,
  NominalDiffTime,
  TimeZone (TimeZone),
  UTCTime (utctDay),
  Year,
  defaultTimeLocale,
  diffUTCTime,
  formatTime,
  fromGregorian,
  getCurrentTime,
  gregorianMonthLength,
  toGregorian,
  utcToZonedTime,
 )
import Data.Time.Calendar.WeekDate (toWeekDate)
import GHC.Generics (Generic)
import Network.HTTP.Req (
  NoReqBody (NoReqBody),
  POST (POST),
  ReqBodyJson (ReqBodyJson),
  defaultHttpConfig,
  https,
  ignoreResponse,
  jsonResponse,
  req,
  responseBody,
  runReq,
  (/:),
  (=:),
 )
import System.Environment (getEnv)
import Prelude hiding (unlines)

data DaytimeDataResults = DaytimeDataResults
  { sunrise :: UTCTime
  , sunset :: UTCTime
  }
  deriving (Show, Generic)

instance FromJSON DaytimeDataResults

instance ToJSON DaytimeDataResults

newtype DaytimeData = DaytimeData
  { results :: DaytimeDataResults
  }
  deriving (Show, Generic)

instance FromJSON DaytimeData

instance ToJSON DaytimeData

data SunriseSunsetAPIDate = Today | Yesterday

getDaytimeData :: (Double, Double) -> SunriseSunsetAPIDate -> IO DaytimeData
getDaytimeData (lat, lng) date =
  let dateText = case date of
        Today -> "today"
        Yesterday -> "yesterday"

      url = https "api.sunrise-sunset.org" /: "json"
      options =
        mconcat
          [ "lat" =: lat
          , "lng" =: lng
          , "formatted" =: (0 :: Int)
          , "date" =: (dateText :: Text)
          ]
   in runReq defaultHttpConfig $ responseBody <$> req POST url NoReqBody jsonResponse options

getDayLength :: DaytimeDataResults -> NominalDiffTime
getDayLength DaytimeDataResults {..} = diffUTCTime sunset sunrise

data TelegramSendMessageBody = TelegramSendMessageBody
  { chat_id :: Text
  , disable_notification :: Bool
  , text :: Text
  }
  deriving (Show, Generic)

instance ToJSON TelegramSendMessageBody

sendTelegramMessage :: Text -> Text -> Text -> IO ()
sendTelegramMessage botKey chatId msg =
  let url = https "api.telegram.org" /: botKey /: "sendMessage"
      body =
        TelegramSendMessageBody
          { chat_id = chatId
          , disable_notification = True
          , text = msg
          }
   in void $ runReq defaultHttpConfig $ req POST url (ReqBodyJson body) ignoreResponse mempty

daytimeDataMessage :: Text -> TimeZone -> DaytimeDataResults -> DaytimeDataResults -> Text
daytimeDataMessage cityName timeZone daytimeDataResultsToday daytimeDataResultsYesterday =
  let dayLengthToday = getDayLength daytimeDataResultsToday
      dayLengthYesterday = getDayLength daytimeDataResultsYesterday
      change = dayLengthToday - dayLengthYesterday
   in cityName
        <> "\n"
        <> unlines
          ( pack
              <$> [ "Sunrise: " ++ formatTime defaultTimeLocale "%H:%M:%S" (utcToZonedTime timeZone $ sunrise daytimeDataResultsToday)
                  , "Sunset: " ++ formatTime defaultTimeLocale "%H:%M:%S" (utcToZonedTime timeZone $ sunset daytimeDataResultsToday)
                  , "Length: " ++ formatTime defaultTimeLocale "%0H:%0M:%0S" dayLengthToday
                  , "Change: " ++ formatTime defaultTimeLocale (diffSign change : "%0H:%0M:%0S") (abs change)
                  ]
          )
  where
    diffSign :: NominalDiffTime -> Char
    diffSign time
      | (> 0) time = '+'
      | otherwise = '-'

data Config = Config
  { telegramBotKey :: Text
  , chatId :: Text
  }

readConfig :: IO Config
readConfig = do
  botKey <- getEnvText "BOT_KEY"
  chatId <- getEnvText "CHAT_ID"
  pure (Config botKey chatId)
  where
    getEnvText key = pack <$> getEnv key

isDaylightSavingTime :: Day -> Bool
isDaylightSavingTime day =
  let (year, _, _) = toGregorian day
      lastSundayOfMarch = lastSundayOfMonth year 3
      lastSundayOfOctober = lastSundayOfMonth year 10
   in day > lastSundayOfMarch && day < lastSundayOfOctober

lastSundayOfMonth :: Year -> MonthOfYear -> Day
lastSundayOfMonth year month = maximum sundaysInMonth
  where
    monthDays = [fromGregorian year month day | day <- [1 .. gregorianMonthLength year month]]
    monthDaysWithWeekDayNumbers =
      [ let (_, _, weekDayNumber) = toWeekDate day
         in (day, weekDayNumber)
      | day <- monthDays
      ]
    sundaysInMonth = fst <$> filter (\(_, weekDayNumber) -> weekDayNumber == 7) monthDaysWithWeekDayNumbers

main :: IO ()
main = do
  Config {telegramBotKey, chatId} <- readConfig
  currentDay <- utctDay <$> getCurrentTime
  let timeZone = if isDaylightSavingTime currentDay then timeZoneEEST else timeZoneEST

  for_ cities $ \(cityName, coords) -> do
    DaytimeData dayTimeDataToday <- getDaytimeData coords Today
    DaytimeData dayTimeDataYesterday <- getDaytimeData coords Yesterday
    let msg =
          daytimeDataMessage
            cityName
            timeZone
            dayTimeDataToday
            dayTimeDataYesterday

    sendTelegramMessage telegramBotKey chatId msg
  where
    cities =
      [ ("Tampere", (61.5007833595595, 23.81062154454931))
      , ("Helsinki", (60.19079404209167, 24.96228363735172))
      ]
    timeZoneEST = TimeZone 120 False "EST"
    timeZoneEEST = TimeZone 180 True "EEST"
