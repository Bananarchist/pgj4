{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
module WSResponse (WSResponse(..), Match(..)) where

import Data.Aeson
import Data.Text (Text, unlines)
import GHC.Generics

data Match = Match
  { name :: Text
  , uuid :: Text
  }
  deriving (Show, Generic)

data WSResponse
  = UserJoined Text
  | NameSet Text
  | UserList [Text]
  | GameAdded Text
  | Failed Text
  | GameList [Match]
  deriving (Show)

instance ToJSON WSResponse where
  toJSON (UserJoined t) = object ["event" .= ("user-joined" :: Text), "message" .= t]
  toJSON (NameSet t) = object ["event" .= ("name-set" :: Text), "message" .= t]
  toJSON (UserList ts) = object ["info" .= ("user-list" :: Text), "data" .= ts]
  toJSON (GameAdded t) = object ["event" .= ("game-added" :: Text), "message" .= t]
  toJSON (Failed t) = object ["error" .= t]
  toJSON (GameList ts) = object ["info" .= ("game-list" :: Text), "data" .= ts]

instance ToJSON Match
