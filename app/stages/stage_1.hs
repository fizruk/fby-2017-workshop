{-# OPTIONS_GHC -Wall #-}
module Stage1 where

import qualified Telegram.Bot.API as Telegram
import Telegram.Bot.Simple

type Model = ()

data Action = NoOp

bot :: BotApp Model Action
bot = BotApp
  { botInitialModel = ()
  , botAction = flip handleUpdate
  , botHandler = handleAction
  , botJobs = []
  }
  where
    handleUpdate :: Model -> Telegram.Update -> Maybe Action
    handleUpdate _model _update = Nothing

    handleAction :: Action -> Model -> Eff Action Model
    handleAction _action model = pure model

run :: Telegram.Token -> IO ()
run token = do
  env <- Telegram.defaultTelegramClientEnv token
  startBot_ bot env
