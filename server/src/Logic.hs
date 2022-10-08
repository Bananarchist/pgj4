{-# LANGUAGE NamedFieldPuns #-}

module Logic (Action (..), Army (..), GameState (..), Color (..), Piece (..), TurnResult (..), Player (..), TurnNumber (..), PlayerRole (..), nextGameState, resultOfAction, setPieceAt, getPieceAt, newPlayer) where

import Data.Text (Text)

data Color
  = Orange
  | Blue
  deriving (Eq, Show)

data Piece
  = Honest Color
  | Deceitful Color
  | Obliterated
  deriving (Eq, Show)

data Army = Army Piece Piece Piece Piece Piece deriving (Eq, Show)

data Player = Player
  { army :: Army,
    health :: Int
  }
  deriving (Eq, Show)

data TurnNumber = First | Second

data PlayerRole = Host | Opponent

data TurnAdvanced = TurnAdvanced

data ProcessingTurn = ProcessingTurn

data GameState
  = PlacingArmy Player Player
  | Playing PlayerRole TurnNumber [TurnResult] Player Player
  | Complete PlayerRole [TurnResult] Player Player

type Spot = Int

type Target = Spot

type Source = Spot

data Action
  = Shoot Target
  | Swap Source Target
  deriving (Eq, Show)

data TurnResult
  = Shot Target
  | Damaged Target
  | Blocked Target
  | Moved Source Target
  | Swapped Source Target
  deriving (Eq, Show)

trueColorOf :: Piece -> Maybe Color
trueColorOf (Honest c) = Just c
trueColorOf (Deceitful c) = if c == Orange then Just Blue else Just Orange
trueColorOf Obliterated = Nothing

getPieceAt :: Spot -> Army -> Piece
getPieceAt 1 (Army p _ _ _ _) = p
getPieceAt 2 (Army _ p _ _ _) = p
getPieceAt 3 (Army _ _ p _ _) = p
getPieceAt 4 (Army _ _ _ p _) = p
getPieceAt 5 (Army _ _ _ _ p) = p
getPieceAt _ _ = Obliterated

setPieceAt :: Piece -> Spot -> Army -> Army
setPieceAt p 1 (Army _ a b c d) = Army p a b c d
setPieceAt p 2 (Army a _ b c d) = Army a p b c d
setPieceAt p 3 (Army a b _ c d) = Army a b p c d
setPieceAt p 4 (Army a b c _ d) = Army a b c p d
setPieceAt p 5 (Army a b c d _) = Army a b c d p
setPieceAt _ _ army = army

obliteratePiece :: Target -> Army -> Army
obliteratePiece = setPieceAt Obliterated

swapPieces :: Source -> Target -> Army -> Army
swapPieces src tar army =
  setPieceAt (getPieceAt tar army) src $ setPieceAt (getPieceAt src army) tar army

playerPieceAt :: Spot -> Player -> Piece
playerPieceAt spot Player {army} = getPieceAt spot army

resultOfAction :: Action -> Player -> Player -> TurnResult
resultOfAction (Shoot target) actor defender =
  let dColor = trueColorOf $ playerPieceAt target defender
      aColor = trueColorOf $ playerPieceAt target actor
   in case (dColor, aColor == dColor) of
        (Nothing, _) -> Damaged target
        (_, True) -> Blocked target
        (_, False) -> Shot target
resultOfAction (Swap source target) Player {army} _
  | getPieceAt source army == Obliterated = Moved target source
  | getPieceAt target army == Obliterated = Moved source target
  | otherwise = Swapped source target

updateHistory :: TurnResult -> GameState -> GameState
updateHistory result (Playing pr tn trs hp op) = Playing pr tn (result : trs) hp op
updateHistory _ gs = gs

updateArmies :: TurnResult -> GameState -> GameState
updateArmies (Shot t) (Playing Host tn trs hp op@Player {army}) = Playing Host tn trs hp (op {army = obliteratePiece t army})
updateArmies (Shot t) (Playing Opponent tn trs hp@Player {army} op) = Playing Opponent tn trs (hp {army = obliteratePiece t army}) op
updateArmies (Swapped s t) (Playing Host tn trs hp@Player {army} op) = Playing Host tn trs hp {army = swapPieces s t army} op
updateArmies (Swapped s t) (Playing Opponent tn trs hp op@Player {army}) = Playing Opponent tn trs hp op {army = swapPieces s t army}
updateArmies (Moved s t) (Playing Host tn trs hp@Player {army} op) = Playing Host tn trs hp {army = swapPieces s t army} op
updateArmies (Moved s t) (Playing Opponent tn trs hp op@Player {army}) = Playing Opponent tn trs hp op {army = swapPieces s t army}
updateArmies _ gs = gs

updateHealth :: TurnResult -> GameState -> GameState
updateHealth (Damaged t) (Playing Host tn trs hp op@Player {health}) = Playing Host tn trs hp op {health = health - 1}
updateHealth (Damaged t) (Playing Opponent tn trs hp@Player {health} op) = Playing Opponent tn trs hp {health = health - 1} op
updateHealth _ gs = gs

isDead :: Player -> Bool
isDead Player {health} = health == 0

updateState :: GameState -> GameState
updateState (Playing Host tn trs hp op) = if isDead op then Complete Host trs hp op else Playing Host tn trs hp op
updateState (Playing Opponent tn trs hp op) = if isDead hp then Complete Opponent trs hp op else Playing Host tn trs hp op
updateState gs = gs

updateTurnNumber :: GameState -> GameState
updateTurnNumber (Playing Host First trs hp op) = Playing Host Second trs hp op
updateTurnNumber (Playing Host Second trs hp op) = Playing Opponent First trs hp op
updateTurnNumber (Playing Opponent First trs hp op) = Playing Opponent Second trs hp op
updateTurnNumber (Playing Opponent Second trs hp op) = Playing Host First trs hp op
updateTurnNumber gs = gs

nextGameState :: TurnResult -> GameState -> GameState
nextGameState result gs =
  updateTurnNumber $
    updateState $
      updateHealth result $
        updateArmies result $
          updateHistory result gs

-- Defaults
newArmy :: Army
newArmy = Army Obliterated Obliterated Obliterated Obliterated Obliterated

newHealth :: Int
newHealth = 5

newPlayer :: Player
newPlayer = Player newArmy newHealth
