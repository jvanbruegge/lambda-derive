module Lambda where

import Aws.Lambda (Context)
import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)
import Lib (runParenthesizer)

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
        body :: String
      }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

handler :: Event -> Context -> IO (Either String Response)
handler MkEvent {body, path, httpMethod} context
  | path == "/derive",
    Just expr <- body,
    httpMethod == "POST" = pure $ case runParenthesizer expr of
    Right s -> Right MkResponse {statusCode = 200, body = s}
    Left e -> Right MkResponse {statusCode = 400, body = e}
  | otherwise =
    pure $
      Right
        MkResponse
          { statusCode = 404,
            body = "Not found"
          }
