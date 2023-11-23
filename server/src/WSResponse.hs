{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module WSResponse (WSResponse (..), User (..), Match (..)) where

import Data.Aeson
import Data.Text (Text, unlines)
import GHC.Generics
import qualified Logic

data Match = Match
  { name :: Text,
    uuid :: Text
  }
  deriving (Show, Generic)

data User = User Text Text deriving (Show)

data WSResponse
  = UserJoined Text
  | NameSet Text
  | UserList [User]
  | GameAdded Text Text
  | PiecesRequested Int
  | GameBegun Int Logic.Army
  | NewGameState Logic.TurnResult Logic.Player Logic.Player
  | ActionRequested
  | Failed Text
  | GameList [Match]
  | Chat Text Text Text
  deriving (Show)

instance ToJSON WSResponse where
  toJSON (UserJoined t) = object ["event" .= ("user-joined" :: Text), "message" .= t]
  toJSON (NameSet t) = object ["event" .= ("name-set" :: Text), "message" .= t]
  toJSON (UserList ts) = object ["info" .= ("user-list" :: Text), "data" .= ts]
  toJSON (GameAdded gid name) = object ["event" .= ("game-added" :: Text), "name" .= name, "id" .= gid]
  toJSON (PiecesRequested t) = object ["event" .= ("pieces-requested" :: Text), "number" .= t]
  toJSON (GameBegun h a) = object ["event" .= ("game-begun" :: Text), "health" .= h, "enemy-army" .= (Logic.perceivedColorOf <$> Logic.armyToList a)]
  toJSON (Failed t) = object ["error" .= t]
  toJSON ActionRequested = object ["event" .= ("action-requested" :: Text)]
  toJSON (NewGameState trs recvPlayer otherPlayer) = object ["event" .= ("new-game-state" :: Text), "turn-result" .= trs, "you" .= recvPlayer, "them" .= otherPlayer]
  toJSON (GameList ts) = object ["info" .= ("game-list" :: Text), "data" .= ts]
  toJSON (Chat cid msg time) = object ["event" .= ("chat" :: Text), "sender_id" .= cid, "message" .= msg, "time" .= time]

instance ToJSON Match

instance ToJSON User where
  toJSON (User sn uuid) = object ["username" .= sn, "userId" .= uuid]

instance ToJSON Logic.Color where
  toJSON Logic.Blue = String ("blue" :: Text)
  toJSON Logic.Orange = String ("orange" :: Text)

instance ToJSON Logic.Piece where
  toJSON (Logic.Honest c) = object ["honest" .= True, "color" .= c]
  toJSON (Logic.Deceitful c) = object ["honest" .= False, "color" .= c]
  toJSON Logic.Obliterated = Null

instance ToJSON Logic.Army where
  toJSON = toJSONList . Logic.armyToList

instance ToJSON Logic.Player

instance ToJSON Logic.TurnResult where
  toJSON (Logic.Shot target) =
    object
      ["action" .= ("shot" :: Text), "target" .= target]
  toJSON (Logic.Damaged target) =
    object
      ["action" .= ("damaged" :: Text), "target" .= target]
  toJSON (Logic.Blocked source) =
    object
      ["action" .= ("blocked" :: Text), "source" .= source]
  toJSON (Logic.Moved source target) =
    object
      ["action" .= ("moved" :: Text), "source" .= source, "target" .= target]
  toJSON (Logic.Swapped source target) =
    object
      ["action" .= ("swapped" :: Text), "source" .= source, "target" .= target]
