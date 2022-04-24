module NoGFXTests exposing (..)

import BattleLogic as BL exposing (Army(..), Honesty(..), Hue(..), Piece(..))
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import NoGFX
import Test exposing (..)


defaultArmy =
    Army Obliterated (Piece Orange Authentic) Obliterated Obliterated Obliterated


defaultPlayer =
    BL.initialPlayerModel defaultArmy


defaultBattleModel =
    BL.initialBattleModel defaultPlayer defaultPlayer


defaultBattleState =
    BL.Battle defaultBattleModel


suite : Test
suite =
    describe "In a game"
        [ describe "During competition"
            [ describe "Active player"
                [ test "Is informed of their turn" <|
                    \_ ->
                        let
                            bm =
                                BL.BattleModel defaultPlayer defaultPlayer (BL.Player2 BL.Second) []

                            bs =
                                BL.Battle bm

                            act1 =
                                BL.Switch 2 1

                            nbs1 =
                                BL.updateBattleState act1 bs
                        in
                        Expect.equal
                            "Now act"
                            (nbs1
                                |> NoGFX.view
                                |> List.head
                                |> Maybe.withDefault "Failed"
                            )
                , test "Gets results of piece move" <|
                    \_ ->
                        let
                            bs =
                                defaultBattleState

                            act1 =
                                BL.Switch 2 1

                            nbs1 =
                                BL.updateBattleState act1 bs
                        in
                        Expect.equal
                            "Moved Authentic Orange from second position to first"
                            (nbs1
                                |> NoGFX.view
                                |> List.head
                                |> Maybe.withDefault "Failed"
                            )
                , test "Gets results of piece swap" <|
                    \_ ->
                        let
                            p =
                                Army (Piece Blue Deceitful) (Piece Orange Authentic) Obliterated Obliterated Obliterated
                                    |> BL.initialPlayerModel

                            bm =
                                BL.initialBattleModel p p

                            bs =
                                BL.Battle bm

                            act1 =
                                BL.Switch 2 1

                            nbs1 =
                                BL.updateBattleState act1 bs
                        in
                        Expect.equal
                            "Swapped Authentic Orange in second position with Deceitful Blue in first position"
                            (nbs1
                                |> NoGFX.view
                                |> List.head
                                |> Maybe.withDefault "Failed"
                            )
                , test "Gets results of fire failure" <|
                    \_ ->
                        let
                            p1 =
                                defaultArmy |> BL.initialPlayerModel

                            p2 =
                                Army Obliterated (Piece Orange Authentic) Obliterated Obliterated Obliterated |> BL.initialPlayerModel

                            bm =
                                BL.initialBattleModel p1 p2

                            bs =
                                BL.Battle bm

                            act1 =
                                BL.Fire 2

                            nbs1 =
                                BL.updateBattleState act1 bs
                        in
                        Expect.equal
                            "Orange firing from second position was blocked!"
                            (nbs1
                                |> NoGFX.view
                                |> List.head
                                |> Maybe.withDefault "Failed"
                            )
                , test "Gets results of fire success" <|
                    \_ ->
                        let
                            p1 =
                                defaultArmy |> BL.initialPlayerModel

                            p2 =
                                Army Obliterated (Piece Blue Deceitful) Obliterated Obliterated Obliterated |> BL.initialPlayerModel

                            bm =
                                BL.initialBattleModel p1 p2

                            bs =
                                BL.Battle bm

                            act1 =
                                BL.Fire 2

                            nbs1 =
                                BL.updateBattleState act1 bs
                        in
                        Expect.equal
                            "Orange firing from second position obliterated opposing Blue!"
                            (nbs1
                                |> NoGFX.view
                                |> List.head
                                |> Maybe.withDefault "Failed"
                            )
                , test "Gets results of fire damage" <|
                    \_ ->
                        let
                            p1 =
                                defaultArmy |> BL.initialPlayerModel

                            p2 =
                                Army Obliterated Obliterated (Piece Blue Deceitful) Obliterated Obliterated |> BL.initialPlayerModel

                            bm =
                                BL.initialBattleModel p1 p2

                            bs =
                                BL.Battle bm

                            act1 =
                                BL.Fire 2

                            nbs1 =
                                BL.updateBattleState act1 bs
                        in
                        Expect.equal
                            "Fired for one damage!"
                            (nbs1
                                |> NoGFX.view
                                |> List.head
                                |> Maybe.withDefault "Failed"
                            )
                ]
            , describe "Inactive player"
                [ test "Is informed it is not their turn" <|
                    \_ ->
                        let
                            fkarmy =
                                Army Obliterated (Piece Orange Authentic) Obliterated Obliterated Obliterated

                            p =
                                fkarmy
                                    |> BL.initialPlayerModel

                            bm =
                                BL.BattleModel p p (BL.Player1 BL.Second) []

                            bs =
                                BL.Battle bm

                            act1 =
                                BL.Switch 2 1

                            nbs1 =
                                BL.updateBattleState act1 bs
                        in
                        Expect.equal
                            "Now wait and see"
                            (nbs1
                                |> NoGFX.view
                                |> List.head
                                |> Maybe.withDefault "Failed"
                            )
                , test "Gets summary of piece movement" <|
                    \_ ->
                        let
                            bm =
                                BL.BattleModel defaultPlayer defaultPlayer (BL.Player2 BL.First) []

                            bs =
                                BL.Battle bm

                            act1 =
                                BL.Switch 2 1

                            nbs1 =
                                BL.updateBattleState act1 bs
                        in
                        Expect.equal
                            "Orange moved from fourth position to fifth"
                            (nbs1
                                |> NoGFX.view
                                |> List.head
                                |> Maybe.withDefault "Failed"
                            )
                , test "Gets summary of piece swap" <|
                    \_ ->
                        let
                            p =
                                Army (Piece Blue Deceitful) (Piece Orange Authentic) Obliterated Obliterated Obliterated
                                    |> BL.initialPlayerModel

                            bm =
                                BL.BattleModel p p (BL.Player2 BL.First) []

                            bs =
                                BL.Battle bm

                            act1 =
                                BL.Switch 2 1

                            nbs1 =
                                BL.updateBattleState act1 bs
                        in
                        Expect.equal
                            "Orange in fourth position swapped places with Orange in fifth position"
                            (nbs1
                                |> NoGFX.view
                                |> List.head
                                |> Maybe.withDefault "Failed"
                            )
                , test "Gets summary of fire evasion" <|
                    \_ ->
                        let
                            p1 =
                                defaultArmy |> BL.initialPlayerModel

                            p2 =
                                Army Obliterated (Piece Orange Authentic) Obliterated Obliterated  Obliterated |> BL.initialPlayerModel

                            bm =
                                BL.initialBattleModel p1 p2

                            bs =
                                BL.Battle { bm | phase = BL.Player2 BL.First }

                            act1 =
                                BL.Fire 2

                            nbs1 =
                                BL.updateBattleState act1 bs
                        in
                        Expect.equal
                            "Orange in fourth position evaded obliteration!"
                            (nbs1
                                |> NoGFX.view
                                |> List.head
                                |> Maybe.withDefault "Failed"
                            )
                , test "Gets summary of fire destruction" <|
                    \_ ->
                        let
                            p1 =
                                defaultArmy |> BL.initialPlayerModel

                            p2 =
                                Army Obliterated (Piece Blue Deceitful) Obliterated Obliterated Obliterated |> BL.initialPlayerModel

                            bm =
                                BL.initialBattleModel p1 p2

                            bs =
                                BL.Battle { bm | phase = BL.Player2 BL.First }

                            act1 =
                                BL.Fire 2

                            nbs1 =
                                BL.updateBattleState act1 bs
                        in
                        Expect.equal
                            "Blue firing from fourth position obliterated opposing Orange!"
                            (nbs1
                                |> NoGFX.view
                                |> List.head
                                |> Maybe.withDefault "Failed"
                            )
                , test "Gets summary of fire damage taken" <|
                    \_ ->
                        let
                            p1 =
                                defaultArmy |> BL.initialPlayerModel

                            p2 =
                                Army Obliterated Obliterated (Piece Blue Deceitful) Obliterated Obliterated |> BL.initialPlayerModel

                            bm =
                                BL.initialBattleModel p1 p2

                            bs =
                                BL.Battle { bm | phase = BL.Player2 BL.First }

                            act1 =
                                BL.Fire 3

                            nbs1 =
                                BL.updateBattleState act1 bs
                        in
                        Expect.equal
                            "Took one damage!"
                            (nbs1
                                |> NoGFX.view
                                |> List.head
                                |> Maybe.withDefault "Failed"
                            )
                ]
            ]
        ]
