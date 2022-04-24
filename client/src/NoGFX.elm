module NoGFX exposing (..)

import Basics.Extra exposing (flip)
import BattleLogic exposing (BattleState(..))
import Maybe.Extra


ordinal : Int -> String
ordinal i =
    case i of
        1 ->
            "first"

        2 ->
            "second"

        3 ->
            "third"

        4 ->
            "fourth"

        5 ->
            "fifth"

        _ ->
            String.fromInt i ++ "th"


viewRecap : BattleState -> Bool -> BattleLogic.TurnRecap -> String
viewRecap bs isAttacker tr =
    let
        pieceDescriptor spot army =
            if isAttacker then
                [ BattleLogic.getHonestyAt spot army
                    |> Maybe.map BattleLogic.honestyString
                , BattleLogic.getHueAt True spot army
                    |> Maybe.map BattleLogic.hueString
                ]
                    |> Maybe.Extra.values
                    |> String.join " "

            else
                BattleLogic.getHueAt False spot army
                    |> Maybe.map BattleLogic.hueString
                    |> Maybe.withDefault ""
    in
    if isAttacker then
        case tr of
            BattleLogic.TurnEnded ->
                "Now act"

            BattleLogic.MovedToEmptySpot src dest ->
                String.join " "
                    [ "Moved"
                    , BattleLogic.player1Army bs |> pieceDescriptor dest
                    , "from"
                    , ordinal src
                    , "position to"
                    , ordinal dest
                    ]

            BattleLogic.SwappedPieces src dest ->
                String.join " "
                    [ "Swapped"
                    , BattleLogic.player1Army bs |> pieceDescriptor dest
                    , "in"
                    , ordinal src
                    , "position with"
                    , BattleLogic.player1Army bs |> pieceDescriptor src
                    , "in"
                    , ordinal dest
                    , "position"
                    ]

            BattleLogic.Damaged src ->
                "Fired for one damage!"

            BattleLogic.UnsuccessfulShot src ->
                String.join " "
                    [ BattleLogic.player1Army bs |> BattleLogic.getHueAt True src |> Maybe.map BattleLogic.hueString |> Maybe.withDefault ""
                    , "firing from"
                    , ordinal src
                    , "position was blocked!"
                    ]

            BattleLogic.SuccessfulShot src ->
                let
                    maybeHue =
                        BattleLogic.player1Army bs |> BattleLogic.getHueAt True src
                in
                String.join " "
                    [ maybeHue |> Maybe.map BattleLogic.hueString |> Maybe.withDefault ""
                    , "firing from"
                    , ordinal src
                    , "position obliterated opposing"
                    , maybeHue |> Maybe.map (BattleLogic.oppositeHue >> BattleLogic.hueString) |> Maybe.withDefault ""
                    ]
                    |> flip (++) "!"

            BattleLogic.GameEnded ->
                "Fired for one damage! Victory!"

    else
        case tr of
            BattleLogic.TurnEnded ->
                "Now wait and see"

            BattleLogic.MovedToEmptySpot src dest ->
                String.join " "
                    [ BattleLogic.player2Army bs |> pieceDescriptor dest
                    , "moved from"
                    , BattleLogic.translatedSpot src |> ordinal
                    , "position to"
                    , BattleLogic.translatedSpot dest |> ordinal
                    ]

            BattleLogic.SwappedPieces src dest ->
                String.join " "
                    [ BattleLogic.player2Army bs |> pieceDescriptor dest
                    , "in"
                    , BattleLogic.translatedSpot src |> ordinal
                    , "position swapped places with"
                    , BattleLogic.player2Army bs |> pieceDescriptor src
                    , "in"
                    , BattleLogic.translatedSpot dest |> ordinal
                    , "position"
                    ]

            BattleLogic.UnsuccessfulShot src ->
                String.join " "
                    [ BattleLogic.player2Army bs |> pieceDescriptor src
                    , "in"
                    , BattleLogic.translatedSpot src |> ordinal
                    , "position evaded obliteration!"
                    ]

            BattleLogic.SuccessfulShot src ->
                let
                    maybeHue =
                        BattleLogic.player2Army bs |> BattleLogic.getHueAt True src
                in
                String.join " "
                    [ maybeHue |> Maybe.map BattleLogic.hueString |> Maybe.withDefault ""
                    , "firing from"
                    , BattleLogic.translatedSpot src |> ordinal
                    , "position obliterated opposing"
                    , maybeHue |> Maybe.map (BattleLogic.oppositeHue >> BattleLogic.hueString) |> Maybe.withDefault ""
                    ]
                    |> flip (++) "!"

            BattleLogic.Damaged src ->
                "Took one damage!"
            BattleLogic.GameEnded ->
                "Took one damage and a defeat! You lost!"


view : BattleState -> List String
view bs =
    let
        viewAttackerRecap =
            viewRecap bs True

        viewSpectatorRecap =
            viewRecap bs False
    in
    case bs of
        Battle { phase, recap } ->
            case phase of
                BattleLogic.Player1 _ ->
                    List.map viewAttackerRecap recap

                BattleLogic.Player2 _ ->
                    List.map viewSpectatorRecap recap

        _ ->
            [ "" ]
