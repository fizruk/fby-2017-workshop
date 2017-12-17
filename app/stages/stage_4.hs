{-# OPTIONS_GHC -Wall #-}
module Stage4 where

import Control.Applicative
import Data.Text (Text)
import qualified Data.Text as Text

import qualified Telegram.Bot.API as Telegram
import Telegram.Bot.Simple
import Telegram.Bot.Simple.UpdateParser

type Model = [Text]

data Action
  = NoOp
  | AddItem Text
  | RemoveItem Text
  | Show

bot :: BotApp Model Action
bot = BotApp
  { botInitialModel = []
  , botAction = flip handleUpdate
  , botHandler = handleAction
  , botJobs = []
  }
  where
    handleUpdate :: Model -> Telegram.Update -> Maybe Action
    handleUpdate _model = parseUpdate $
          AddItem    <$> plainText
      <|> Show       <$  command "show"
      <|> RemoveItem <$> command "remove"

    handleAction :: Action -> Model -> Eff Action Model
    handleAction action model = case action of
      NoOp -> pure model
      AddItem item -> (item : model) <# do
        pure Show
      RemoveItem item -> filter (/= item) model <# do
        pure Show
      Show -> model <# do
        replyText (Text.unlines model)
        pure NoOp

run :: Telegram.Token -> IO ()
run token = do
  env <- Telegram.defaultTelegramClientEnv token
  startBot_ bot env
