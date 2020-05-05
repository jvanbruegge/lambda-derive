module Telegram (handleUpdate) where

import Control.Monad.IO.Class (liftIO)
import Data.Maybe (fromMaybe)
import Data.Text (Text, isPrefixOf, pack, unpack)
import Lib (runParenthesizer)
import Web.Telegram.API.Bot (ChatId (..), Message (..), ParseMode (..), SendMessageRequest (..), TelegramClient, Update (..), User (..), chat_id, sendMessageM, sendMessageRequest)

sendMarkdown :: ChatId -> Text -> SendMessageRequest
sendMarkdown i t = (sendMessageRequest i t) {message_parse_mode = Just Markdown}

handleUpdate :: Update -> TelegramClient ()
handleUpdate Update {message = Just m} = do
  let c = ChatId $ chat_id $ chat m
  let content = fromMaybe "" $ text m
  liftIO $ putStrLn $
    "message from " <> fromMaybe "?" (unpack . user_first_name <$> from m)
      <> ": "
      <> unpack content
  if "/start" `isPrefixOf` content
    then do
      rm <-
        sendMessageM $
          sendMessageRequest
            c
            "Hi! I am @HaskellDerivativeBot!\nSend me an arithmetic expression to parse."
      liftIO $ putStrLn $ "Sent geeting: " <> show rm
      pure ()
    else do
      let answer = case runParenthesizer (unpack content) of
            Left err -> sendMarkdown c $ pack $ "Looks like the expression is not well formed:\n```\n" <> err <> "```"
            Right res -> sendMessageRequest c $ pack $ "Here is the expression with parenthesis:\n" <> res
      _ <- sendMessageM answer
      pure ()
handleUpdate u = liftIO $ putStrLn $ "Unhandled message: " <> show u
