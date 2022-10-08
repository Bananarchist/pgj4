{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}

module Unit.LogicSpec (spec) where

import Logic
import Test.Hspec
import Test.QuickCheck

monoArmy :: Color -> Army
monoArmy c = Army (Honest c) (Honest c) (Honest c) (Honest c) (Honest c)

defaultPlayerA = Player {health = 5, army = monoArmy Orange}

defaultPlayerB = Player {health = 1, army = monoArmy Orange}

defaultPlayerC = Player {health = 5, army = monoArmy Blue}

defaultPlayerD = Player {health = 1, army = monoArmy Blue}

swapPlayerA = defaultPlayerA {army = setPieceAt (Honest Blue) 2 (army defaultPlayerA)}

movePlayerA = defaultPlayerA {army = setPieceAt Obliterated 2 (army defaultPlayerA)}

spec = do
  describe "In a competition" $ do
    describe "The active player" $ do
      describe "Can move pieces" $ do
        it "By swapping two pieces" $ do
          let (Playing Host Second (rs : _) Player {army} _) =
                flip Logic.nextGameState (Playing Host First [] swapPlayerA defaultPlayerB) $
                  Logic.resultOfAction (Swap 1 2) swapPlayerA defaultPlayerB
          (rs, army) `shouldBe` (Swapped 1 2, Army (Honest Blue) (Honest Orange) (Honest Orange) (Honest Orange) (Honest Orange))
        it "By moving a piece to a blank spot" $ do
          let (Playing Host Second (rs : _) Player {army} _) =
                flip Logic.nextGameState (Playing Host First [] movePlayerA defaultPlayerB) $
                  Logic.resultOfAction (Swap 1 2) movePlayerA defaultPlayerB
          (rs, army) `shouldBe` (Moved 1 2, Army Obliterated (Honest Orange) (Honest Orange) (Honest Orange) (Honest Orange))
