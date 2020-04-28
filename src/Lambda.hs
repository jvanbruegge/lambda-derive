module Lambda where

import Aws.Lambda (Context)
import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)

data Event = MkEvent {path :: String, body :: Maybe String}
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

data Response = MkResponse {statusCode :: Int, body :: String}
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

handler :: Event -> Context -> IO (Either String Response)
handler MkEvent {body, path} context
  | path == "/derive" =
    pure $
      Right
        MkResponse
          { statusCode = 200,
            body = "You called /derive"
          }
  | otherwise =
    pure $
      Right
        MkResponse
          { statusCode = 404,
            body = "Not found"
          }
