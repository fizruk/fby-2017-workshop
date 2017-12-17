{-# OPTIONS_GHC -Wall #-}
module Stage6 where

import Control.Applicative
import Data.Text (Text)
import qualified Data.Text as Text

import qualified Telegram.Bot.API as Telegram
import Telegram.Bot.Simple
import Telegram.Bot.Simple.UpdateParser

type Model = [Text]

data Action
  = NoOp
  | Start
  | AddItem Text
  | RemoveItem Text
  | Show

startMessage :: Text
startMessage = Text.unlines
 [ "Hi there! I am your personal todo bot :)"
 , ""
 , "With me you can keep track of things to do:"
 , "- just type what you need to do an I'll remember it!"
 , "- use /remove <item> to remove an item"
 , "- use /show to show all current things to do"
 , ""
 , "So what's the first thing on your to do list? :)"
 ]

startMessageKeyboard :: Telegram.ReplyKeyboardMarkup
startMessageKeyboard = Telegram.ReplyKeyboardMarkup
  { Telegram.replyKeyboardMarkupKeyboard =
      [ [ "Buy milk", "Bake a cake" ]
      , [ "Build a house", "Raise a son", "Plant a tree" ] ]
  , Telegram.replyKeyboardMarkupResizeKeyboard = Just True
  , Telegram.replyKeyboardMarkupOneTimeKeyboard = Just True
  , Telegram.replyKeyboardMarkupSelective = Just True
  }

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
      <|> Start      <$  command "start"

    handleAction :: Action -> Model -> Eff Action Model
    handleAction action model = case action of
      NoOp -> pure model
      Start -> model <# do
        reply (toReplyMessage startMessage)
          { replyMessageReplyMarkup = Just (Telegram.SomeReplyKeyboardMarkup startMessageKeyboard) }
        pure NoOp
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
