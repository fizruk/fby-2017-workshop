{-# OPTIONS_GHC -Wall #-}
module Stage7 where

import Control.Applicative
import Data.Monoid
import Data.Text (Text)
import qualified Data.Text as Text
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap

import qualified Telegram.Bot.API as Telegram
import Telegram.Bot.Simple
import Telegram.Bot.Simple.UpdateParser

data Model = Model
  { todoLists   :: HashMap Text [Text]
  , currentList :: Text
  }

initialModel :: Model
initialModel = Model HashMap.empty defaultListName

defaultListName :: Text
defaultListName = "Default"

addItem :: Text -> Model -> Model
addItem item model = model
  { todoLists = HashMap.insertWith (++) (currentList model) [item] (todoLists model) }

removeItem :: Text -> Model -> Model
removeItem item model = model
  { todoLists = HashMap.adjust (filter (/= item)) (currentList model) (todoLists model) }

getCurrentList :: Model -> [Text]
getCurrentList model = case mlist of
  Nothing    -> []
  Just items -> items
  where
    mlist = HashMap.lookup (currentList model) (todoLists model)

itemsToText :: [Text] -> Text
itemsToText items = case items of
  [] -> "There are no items."
  _  -> Text.unlines items

data Action
  = NoOp
  | Start
  | AddItem Text
  | RemoveItem Text
  | SwitchTo Text
  | ShowAll
  | Show

startMessage :: Text
startMessage = Text.unlines
 [ "Hi there! I am your personal todo bot :)"
 , ""
 , "With me you can keep track of things to do:"
 , "- just type what you need to do an I'll remember it!"
 , "- use /remove <item> to remove an item"
 , "- use /show to to show all things to do in the current list"
 , "- use /show_all to show all items in all lists"
 , "- switch between multiple to do lists with /switch_to <list>"
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
  { botInitialModel = initialModel
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
      <|> SwitchTo   <$> command "switch_to"
      <|> ShowAll    <$  command "show_all"

    handleAction :: Action -> Model -> Eff Action Model
    handleAction action model = case action of
      NoOp -> pure model
      Start -> model <# do
        reply (toReplyMessage startMessage)
          { replyMessageReplyMarkup = Just (Telegram.SomeReplyKeyboardMarkup startMessageKeyboard) }
        pure NoOp
      AddItem item -> addItem item model <# do
        pure Show
      RemoveItem item -> removeItem item model <# do
        pure Show
      Show -> model <# do
        replyText (itemsToText (getCurrentList model))
        pure NoOp
      SwitchTo name -> model { currentList = name } <# do
        replyText $
          "Switching to list «" <> name <> "»"
        return Show
      ShowAll -> model <# do
        replyText (itemsToText (concat (todoLists model)))
        return NoOp

run :: Telegram.Token -> IO ()
run token = do
  env <- Telegram.defaultTelegramClientEnv token
  startBot_ bot env
