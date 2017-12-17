{-# OPTIONS_GHC -Wall #-}
module Stage2 where

import qualified Telegram.Bot.API as Telegram
import Telegram.Bot.Simple

type Model = ()

data Action
  = NoOp
  | UpdateIncoming

bot :: BotApp Model Action
bot = BotApp
  { botInitialModel = ()
  , botAction = flip handleUpdate
  , botHandler = handleAction
  , botJobs = []
  }
  where
    handleUpdate :: Model -> Telegram.Update -> Maybe Action
    handleUpdate _model _update = Just UpdateIncoming

    handleAction :: Action -> Model -> Eff Action Model
    handleAction action model = case action of
      NoOp -> pure model
      UpdateIncoming -> model <# do
        replyText "Oookay."
        pure NoOp

run :: Telegram.Token -> IO ()
run token = do
  env <- Telegram.defaultTelegramClientEnv token
  startBot_ bot env
