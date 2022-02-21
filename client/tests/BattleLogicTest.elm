module BattleLogicTest exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)

import BattleLogic as BL exposing (Hue(..), Honesty(..), Army(..), Piece(..), Spot, PlayerModel, BattleAction(..), TurnRecap(..))

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
                            testCase : (Army, List TurnRecap)
                            testCase = 
                                case nbs of
                                    BL.Battle nbm ->
                                        (nbm.p1.army, nbm.recap)
                                    _ ->
                                        (fkarmy, [])
                        in
                        Expect.all
                            [ Tuple.first >> Expect.equal 
                                (Army (Piece Orange Authentic) Obliterated (Piece Blue Deceitful) Obliterated Obliterated)
                            , Tuple.second >> Expect.equal [ SwappedPieces 1 3 ]
                            ] testCase
                    
                , test "To empty spots" <|
                    \_ ->
                        let
                            fkarmy = Army Obliterated (Piece Orange Authentic) Obliterated Obliterated Obliterated 
                            p = fkarmy
                                |> BL.initialPlayerModel
                            bm = BL.initialBattleModel p p
                            bs = BL.Battle bm
                            act = Switch 2 1
                            nbs = BL.updateBattleState act bs
                            testCase : (Army, List TurnRecap)
                            testCase = 
                                case nbs of
                                    BL.Battle nbm ->
                                        (nbm.p1.army, nbm.recap)
                                    _ ->
                                        (fkarmy, [])
                        in
                        Expect.all
                            [ Tuple.first >> Expect.equal 
                                (Army (Piece Orange Authentic) Obliterated Obliterated Obliterated Obliterated)
                            , Tuple.second >> Expect.equal
                                [ MovedToEmptySpot 2 1 ]
                            ] testCase
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
                            testCase : (Army, List TurnRecap)
                            testCase = 
                                case nbs of
                                    BL.Battle nbm ->
                                        (nbm.p2.army, nbm.recap)
                                    _ ->
                                        (p2.army, [])
                        in
                        Expect.all 
                            [ Tuple.first >> Expect.equal 
                                (Army Obliterated (Piece Blue Authentic) Obliterated (Piece Orange Authentic) Obliterated)
                            , Tuple.second >> Expect.equal
                                [ UnsuccessfulShot 2 ]
                            ] testCase

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
                            testCase : (Army, List TurnRecap)
                            testCase = 
                                case nbs of
                                    BL.Battle nbm ->
                                        (nbm.p2.army, nbm.recap)
                                    _ ->
                                        (p2.army, [])
                        in
                        Expect.all
                            [ Tuple.first >> Expect.equal 
                                (Army Obliterated (Piece Blue Authentic) Obliterated Obliterated Obliterated)
                            , Tuple.second >> Expect.equal
                                [ SuccessfulShot 2 ]
                            ] testCase
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
                            testCase : (PlayerModel, List TurnRecap)
                            testCase = 
                                case nbs of
                                    BL.Battle nbm ->
                                        (nbm.p2, nbm.recap)
                                    _ ->
                                        (p2, [])
                        in
                        Expect.all
                            [ Tuple.first >> Expect.all
                                [ .army >> Expect.equal 
                                    (Army Obliterated (Piece Blue Authentic) Obliterated Obliterated Obliterated)
                                , .health >> Expect.equal 4 
                                ]
                            , Tuple.second >> Expect.equal
                                [ Damaged 2 ]
                            ] testCase
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
            , describe "Can take two actions"
                [ test "And then it is the other player's turn" <|
                    \_ ->
                        let 
                            fkarmy = Army Obliterated (Piece Orange Authentic) Obliterated Obliterated Obliterated 
                            p1 = fkarmy
                                |> BL.initialPlayerModel
                            p2 = { p1 | health = 1 }
                            bm = BL.initialBattleModel p1 p2
                            bs = BL.Battle bm
                            act1 = BL.Switch 2 1
                            act2 = BL.Switch 1 4
                            nbs1 = BL.updateBattleState act1 bs
                            nbs2 = BL.updateBattleState act2 nbs1 
                            nbs3 = BL.updateBattleState act1 nbs2
                            nbs4 = BL.updateBattleState act2 nbs3 
                            recap = 
                                case nbs4 of
                                    BL.Battle bmod -> bmod.recap
                                    _ -> []
                            testCase : ((BL.BattleState, BL.BattleState), List TurnRecap)
                            testCase = 
                                ((nbs2, nbs4), recap)
                        in
                        Expect.all
                            [ Tuple.first >> Tuple.first >> BL.player2TurnStart >> Expect.true "Expected to be player 2 turn start" 
                            , Tuple.first >> Tuple.second >> BL.player1TurnStart >> Expect.true "Expected to be player 1 turn start" 
                            , Tuple.second >> Expect.equal 
                                [ TurnEnded 
                                , MovedToEmptySpot 1 4
                                , MovedToEmptySpot 2 1
                                , TurnEnded
                                , MovedToEmptySpot 1 4
                                , MovedToEmptySpot 2 1
                                ]
                            ] testCase
                ]
            ]
        ]


