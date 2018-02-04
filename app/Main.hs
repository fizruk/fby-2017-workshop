module Main where

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

startKeyboard :: [[Telegram.KeyboardButton]]
startKeyboard =
  [ [ "Buy milk", "Купить молоко" ]
  , [ "Посадить дерево", "Вырастить сына", "Построить дом" ]
  ]

bot :: BotApp Model Action
bot = BotApp
  { botInitialModel = []
  , botAction = handleUpdate
  , botHandler = handleAction
  , botJobs = []
  }

handleUpdate :: Telegram.Update -> Model -> Maybe Action
handleUpdate update _model = parseUpdate p update
  where
    p :: UpdateParser Action
    p =   Show       <$  command "show"
      <|> Start      <$  command "start"
      <|> RemoveItem <$> command "remove"
      <|> AddItem    <$> text

handleAction :: Action -> Model -> Eff Action Model
handleAction action model = case action of
  NoOp -> pure model
  Start -> model <# do
    reply ReplyMessage
      { replyMessageText = startMessage
      , replyMessageParseMode = Nothing
      , replyMessageDisableWebPagePreview = Nothing
      , replyMessageDisableNotification = Nothing
      , replyMessageReplyToMessageId = Nothing
      , replyMessageReplyMarkup =
          Just $ Telegram.SomeReplyKeyboardMarkup
            Telegram.ReplyKeyboardMarkup
            { Telegram.replyKeyboardMarkupKeyboard = startKeyboard
            , Telegram.replyKeyboardMarkupResizeKeyboard = Nothing
            , Telegram.replyKeyboardMarkupOneTimeKeyboard = Nothing
            , Telegram.replyKeyboardMarkupSelective = Nothing
            }
      }
    pure NoOp
  AddItem item -> (model ++ [item]) <# do
    pure Show
  Show -> model <# do
    if null model
      then replyText "No items."
      else replyText (Text.unlines model)
    pure NoOp
  RemoveItem item -> filter (/= item) model <# do
    pure Show

run :: Telegram.Token -> IO ()
run token = do
  env <- Telegram.defaultTelegramClientEnv token
  startBot_ (conversationBot Telegram.updateChatId bot) env

main :: IO ()
main = putStrLn "Telegram bot is not implemented yet!"
