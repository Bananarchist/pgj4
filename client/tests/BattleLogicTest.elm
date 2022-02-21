module BattleLogicTest exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)

import BattleLogic as BL exposing (Hue(..), Honesty(..), Army(..), Piece(..), Spot, PlayerModel, BattleAction(..))

suite : Test
suite =
    describe "In a competition"
        [ describe "The active player"
            [ describe "Can move pieces"
                [ test "By swapping two pieces" <|
                    \_ ->
                        let
                            fkarmy = Army (Piece Blue Deceitful) Obliterated (Piece Orange Authentic) Obliterated Obliterated 
                            p = fkarmy
                                |> BL.initialPlayerModel
                            bm = BL.initialBattleModel p p
                            bs = BL.Battle bm
                            act = Switch 1 3
                            nbs = BL.updateBattleState act bs
                            newArmyArrangement = 
                                case nbs of
                                    BL.Battle nbm ->
                                        nbm.p1.army
                                    _ ->
                                        fkarmy
                        in
                        Expect.equal newArmyArrangement (Army (Piece Orange Authentic) Obliterated (Piece Blue Deceitful) Obliterated Obliterated)
                    
                , test "To empty spots" <|
                    \_ ->
                        let
                            fkarmy = Army Obliterated (Piece Orange Authentic) Obliterated Obliterated Obliterated 
                            p = fkarmy
                                |> BL.initialPlayerModel
                            bm = BL.initialBattleModel p p
                            bs = BL.Battle bm
                            act = Switch 1 2
                            nbs = BL.updateBattleState act bs
                            newArmyArrangement = 
                                case nbs of
                                    BL.Battle nbm ->
                                        nbm.p1.army
                                    _ ->
                                        fkarmy
                        in
                        Expect.equal newArmyArrangement (Army (Piece Orange Authentic) Obliterated Obliterated Obliterated Obliterated)
                ]
            , describe "Can fire"
                [ test "And fail to destroy a piece" <|
                    \_ ->
                        let
                            p1 = Army Obliterated (Piece Orange Authentic) Obliterated Obliterated Obliterated 
                                |> BL.initialPlayerModel
                            p2 = Army Obliterated (Piece Blue Authentic) Obliterated (Piece Orange Authentic) Obliterated 
                                |> BL.initialPlayerModel
                            bm = BL.initialBattleModel p1 p2
                            bs = BL.Battle bm
                            act = Fire 2
                            nbs = BL.updateBattleState act bs
                            newArmyArrangement = 
                                case nbs of
                                    BL.Battle nbm ->
                                        nbm.p2.army
                                    _ ->
                                        p2.army
                        in
                        Expect.equal newArmyArrangement (Army Obliterated (Piece Blue Authentic) Obliterated (Piece Orange Authentic) Obliterated)

                , test "And destroy a piece" <|
                    \_ ->
                        let
                            p1 = Army Obliterated (Piece Orange Authentic) Obliterated Obliterated Obliterated 
                                |> BL.initialPlayerModel
                            p2 = Army Obliterated (Piece Blue Authentic) Obliterated (Piece Blue Authentic) Obliterated 
                                |> BL.initialPlayerModel
                            bm = BL.initialBattleModel p1 p2
                            bs = BL.Battle bm
                            act = Fire 2
                            nbs = BL.updateBattleState act bs
                            newArmyArrangement = 
                                case nbs of
                                    BL.Battle nbm ->
                                        nbm.p2.army
                                    _ ->
                                        p2.army
                        in
                        Expect.equal newArmyArrangement (Army Obliterated (Piece Blue Authentic) Obliterated Obliterated Obliterated)
                , test "And cause damage to opponent" <|
                    \_ ->
                        let
                            p1 = Army Obliterated (Piece Orange Authentic) Obliterated Obliterated Obliterated 
                                |> BL.initialPlayerModel
                            p2 = Army Obliterated (Piece Blue Authentic) Obliterated Obliterated Obliterated 
                                |> BL.initialPlayerModel
                            bm = BL.initialBattleModel p1 p2
                            bs = BL.Battle bm
                            act = Fire 2
                            nbs = BL.updateBattleState act bs
                            
                            newP2 = 
                                case nbs of
                                    BL.Battle nbm ->
                                        nbm.p2
                                    _ ->
                                        p2
                        in
                        Expect.all
                            [ .army >> Expect.equal 
                                (Army Obliterated (Piece Blue Authentic) Obliterated Obliterated Obliterated)
                            , .health >> Expect.equal 4 
                            ] newP2
                ]
            , describe "Can win"
                [ test "By causing damage to single health opponent" <|
                    \_ ->
                        let
                            fkarmy = Army Obliterated (Piece Orange Authentic) Obliterated Obliterated Obliterated 
                            p1 = fkarmy
                                |> BL.initialPlayerModel
                            p2 = { p1 | health = 1 }
                            bm = BL.initialBattleModel p1 p2
                            bs = BL.Battle bm
                            act = Fire 2
                            nbs = BL.updateBattleState act bs
                            newP2 = 
                                case nbs of
                                    BL.Battle nbm ->
                                        nbm.p2
                                    _ ->
                                        p2
                        in
                        Expect.equal nbs BL.Player1Victory
                ]
            ]
        ]


