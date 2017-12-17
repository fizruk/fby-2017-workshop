{-# OPTIONS_GHC -Wall #-}
module Stage3 where

import Data.Text

import qualified Telegram.Bot.API as Telegram
import Telegram.Bot.Simple
import Telegram.Bot.Simple.UpdateParser

type Model = ()

data Action
  = NoOp
  | Echo Text

bot :: BotApp Model Action
bot = BotApp
  { botInitialModel = ()
  , botAction = flip handleUpdate
  , botHandler = handleAction
  , botJobs = []
  }
  where
    handleUpdate :: Model -> Telegram.Update -> Maybe Action
    handleUpdate _model = parseUpdate $
      Echo <$> text

    handleAction :: Action -> Model -> Eff Action Model
    handleAction action model = case action of
      NoOp -> pure model
      Echo msg -> model <# do
        replyText msg
        pure NoOp

run :: Telegram.Token -> IO ()
run token = do
  env <- Telegram.defaultTelegramClientEnv token
  startBot_ bot env
