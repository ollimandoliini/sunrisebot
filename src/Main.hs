{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main (main) where

import Control.Monad (void)
import Data.Foldable (for_)
import Data.Text (Text, pack, unlines)
import Data.Time (NominalDiffTime, TimeZone, UTCTime, defaultTimeLocale, diffUTCTime, formatTime, utcToZonedTime)
import GHC.Generics (Generic)
import Prelude hiding (unlines)

import Data.Aeson (FromJSON, ToJSON)
import Network.HTTP.Req (
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

getDaytimeData :: (Double, Double) -> IO DaytimeData
getDaytimeData (lat, lng) =
  let url = https "api.sunrise-sunset.org" /: "json"
      options =
        mconcat
          [ "lat" =: lat
          , "lng" =: lng
          , "formatted" =: (0 :: Int)
          , "date" =: ("today" :: Text)
          ]
   in runReq defaultHttpConfig $ responseBody <$> req POST url (ReqBodyJson ()) jsonResponse options

getDayLength :: DaytimeDataResults -> NominalDiffTime
getDayLength DaytimeDataResults{..} = diffUTCTime sunset sunrise

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

daytimeDataMessage :: Text -> TimeZone -> DaytimeDataResults -> Text
daytimeDataMessage cityName timeZone daytimeDataResults =
  let dayLength = getDayLength daytimeDataResults
   in cityName
        <> "\n"
        <> unlines
          ( pack
              <$> [ "Sunrise: " ++ formatTime defaultTimeLocale "%H:%M:%S" (utcToZonedTime timeZone $ sunrise daytimeDataResults)
                  , "Sunset: " ++ formatTime defaultTimeLocale "%H:%M:%S" (utcToZonedTime timeZone $ sunset daytimeDataResults)
                  , "Length: " ++ formatTime defaultTimeLocale "%H:%M:%S" dayLength
                  ]
          )

data Config = Config
  { telegramBotKey :: Text
  , chatId :: Text
  }

readConfig :: IO Config
readConfig = Config <$> getEnvText "BOT_KEY" <*> getEnvText "CHAT_ID"
 where
  getEnvText key = pack <$> getEnv key

main :: IO ()
main = do
  Config{telegramBotKey, chatId} <- readConfig
  for_ cities $ \(cityName, coords) -> do
    DaytimeData results <- getDaytimeData coords
    let localTimeZone = read "+0200" -- TODO: Check current timezone
        msg = daytimeDataMessage cityName localTimeZone results
    sendTelegramMessage telegramBotKey chatId msg
 where
  cities =
    [ ("Tampere", (61.5007833595595, 23.81062154454931))
    , ("Helsinki", (60.19079404209167, 24.96228363735172))
    ]
