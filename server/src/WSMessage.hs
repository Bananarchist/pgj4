{-# LANGUAGE OverloadedStrings #-}

module WSMessage (WSMessage (..)) where

import Data.Aeson (FromJSON, Value (..), parseJSON, (.:))
import Data.Aeson.Types (Parser, prependFailure, typeMismatch, unexpected)
import Data.Text (Text)
import Data.UUID
import Logic (Action (..), Color (..), Piece (..))

data WSMessage
  = SetName Text
  | HostGame Text
  | ListGames
  | ListUsers
  | JoinGame Text
  | SetPieces [Piece]
  | Play Action
  | Forfeit
  | SendChat Text
  deriving (Eq, Show)

-- - format: {msg: "SetName", name: "name"}
-- - format: {msg: "HostGame"}
-- - format: {msg: "JoinGame", gameId: "guid"}
-- - format: {msg: "SetPieces", pieces: [pieces]}
-- - format: {msg: "Play", action: {type: "swap", source: 1, target: 2}/{type: "shoot", target: 2}
-- - format: {msg: "Forfeit"}
instance FromJSON WSMessage where
  parseJSON v@(Object obj) = do
    msg_type <- (obj .: "msg") :: Parser Text
    case msg_type of
      "SetName" -> SetName <$> obj .: "name"
      "HostGame" -> HostGame <$> obj .: "name"
      "ListGames" -> pure ListGames
      "ListUsers" -> pure ListUsers
      "JoinGame" -> JoinGame <$> obj .: "gameId"
      "SetPieces" -> SetPieces <$> obj .: "pieces"
      "Play" -> Play <$> obj .: "action"
      "Forfeit" -> pure Forfeit
      "SendChat" -> SendChat <$> obj .: "content"
      _ -> prependFailure "Unknown message, " (unexpected v)
  parseJSON invalid =
    prependFailure "Parsing message failed, " (typeMismatch "Object" invalid)

instance FromJSON Action where
  parseJSON v@(Object obj) = do
    act_type <- (obj .: "type") :: Parser Text
    case act_type of
      "swap" -> do
        source <- obj .: "source"
        target <- obj .: "target"
        pure $ Swap source target
      "shoot" -> do
        target <- obj .: "target"
        pure $ Shoot target
      _ -> prependFailure "Unknown action, " (unexpected v)
  parseJSON invalid =
    prependFailure "Parsing action failed, " (typeMismatch "Object" invalid)

instance FromJSON Piece where
  parseJSON (Object obj) = do
    color <- (obj .: "color") :: Parser Text
    is_honest <- (obj .: "honest") :: Parser Bool
    if is_honest
      then
        if color == "blue"
          then pure $ Honest Blue
          else pure $ Honest Orange
      else
        if color == "blue"
          then pure $ Deceitful Blue
          else pure $ Deceitful Orange
  parseJSON invalid =
    prependFailure "Parsing piece failed, " (typeMismatch "Object" invalid)
