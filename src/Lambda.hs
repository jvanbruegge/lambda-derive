module Lambda where

import Aws.Lambda (Context)
import Data.Aeson ((.=), FromJSON, ToJSON, Value, eitherDecode, object)
import Data.ByteString.Lazy (fromStrict)
import Data.Maybe (fromMaybe)
import Data.Text (pack, unpack)
import Data.Text.Encoding (encodeUtf8)
import GHC.Generics (Generic)
import Lib (runParenthesizer)
import Network.HTTP.Client (Manager, newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import System.Environment (getEnv)
import Telegram (handleUpdate)
import Web.Telegram.API.Bot (Token (..), runTelegramClient)

data Event
  = MkEvent
      { path :: String,
        body :: Maybe String,
        httpMethod :: String
      }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

data Response
  = MkResponse
      { statusCode :: Int,
        headers :: Value,
        body :: String
      }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

handler :: Event -> Context -> IO (Either String Response)
handler event context = do
  token <- Token . pack <$> getEnv "TELEGRAM_BOT_TOKEN"
  manager <- newManager tlsManagerSettings
  handler' token manager event context

handler' :: Token -> Manager -> Event -> Context -> IO (Either String Response)
handler' token manager MkEvent {body, path, httpMethod} context
  | path == "/derive",
    Just expr <- body,
    httpMethod == "POST" = pure $ case runParenthesizer expr of
    Right s -> Right MkResponse {statusCode = 200, body = s, headers = object []}
    Left e -> Right MkResponse {statusCode = 200, body = e, headers = object []}
  | path == "/telegram" = putStrLn "Got message from telegram"
    *> case (eitherDecode . fromStrict . encodeUtf8 . pack $ fromMaybe "" body) of
      Left err -> pure $ Right MkResponse {statusCode = 400, body = err, headers = object []}
      Right update -> do
        _ <- runTelegramClient token manager $ handleUpdate update
        pure $
          Right
            MkResponse
              { statusCode = 200,
                headers = object ["Content-Type" .= ("text/plain" :: String)],
                body = "Done"
              }
  | otherwise = do
    putStrLn ("Received unhandled path: " <> path)
    pure $
      Right
        MkResponse
          { statusCode = 200,
            body = "Not found",
            headers = object []
          }
