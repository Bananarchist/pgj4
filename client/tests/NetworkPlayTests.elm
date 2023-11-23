module NetworkPlayTests exposing (suite)

import Expect exposing (Expectation)
import NetworkPlay exposing (..)
import Test exposing (..)
import Fuzz exposing (Fuzzer)
import Fuzz.Extra exposing (battleModel)
import BattleLogic as BL
import ConnectionData exposing (ConnectionData)

battleStateFuzzer : Fuzzer BL.BattleState
battleStateFuzzer =
    Fuzz.oneOf
        [ Fuzz.map (Tuple.pair []) battleModel |> Fuzz.map BL.Player1Victory
        , Fuzz.map (Tuple.pair []) battleModel |> Fuzz.map BL.Player2Victory
        ]

hostFuzzer : Fuzzer Host
hostFuzzer =
    Fuzz.oneOf
        [ Fuzz.constant <| Host "someone"
        , Fuzz.constant <| RemoteHost "someone"
        ]

connectionDataFuzzer : Fuzzer ConnectionData
connectionDataFuzzer =
    Fuzz.map3 ConnectionData.init Fuzz.string Fuzz.int Fuzz.string

gameOverFuzzer : GameOverState -> Fuzzer Model
gameOverFuzzer state =
    Fuzz.map4 GameOver
        (Fuzz.constant state)
        hostFuzzer
        battleStateFuzzer
        connectionDataFuzzer

expectGameOverState : GameOverState -> (Model, Cmd Msg) -> Expectation
expectGameOverState state (model, _) =
    case model of
        GameOver s _ _ _ ->
            Expect.equal state s
            |> Expect.onFail 
                ( String.join "\n"
                    [ "Expected"
                    , Debug.toString state
                    , "but got"
                    , Debug.toString s
                    ]
                )
        _ -> 
            String.join "\n"
                [ "Expected"
                , "GameOver " ++ Debug.toString state ++ " <data> <data> <data>"
                , "but got"
                , Debug.toString model
                ]
                |> Expect.fail 

expectNoOpponent : RemoteForfeitState -> (Model, Cmd Msg) -> Expectation
expectNoOpponent state (model, _) =
    case model of
        NoOpponent s _ ->
            Expect.equal state s
            |> Expect.onFail 
                ( String.join "\n"
                    [ "Expected"
                    , Debug.toString state
                    , "but got"
                    , Debug.toString s
                    ]
                )
        _ ->
            String.join "\n"
                [ "Expected"
                , "NoOpponent " ++ Debug.toString state ++ " <data>"
                , "but got"
                , Debug.toString model
                ]
                |> Expect.fail

suite : Test
suite =
    [ rematchLogicTests
    ]
    |> describe "NetworkPlay"

rematchLogicTests : Test
rematchLogicTests =
    let
        fuzzGameOver string =
            fuzz (gameOverFuzzer ReviewingResults) string
    in
    [ fuzzGameOver "Remote declines rematch on results" <|
        update RemoteDisconnected
            >> expectNoOpponent RemoteForfeitAlert
    , fuzzGameOver "Remote declines rematch on request" <|
        update RequestRematch
            >> Tuple.first
            >> update RemoteDisconnected
            >> expectNoOpponent RemoteDeclinedRematch
    , fuzzGameOver "Mutual rematch desires prompt rematch" <|
        update RequestRematch
            >> Tuple.first
            >> update RemoteRequestedRematch
            >> expectGameOverState Rematch
    , fuzzGameOver "Remote requesting rematch on results" <|
        update RemoteRequestedRematch
            >> expectGameOverState RemoteRematchRequested
    , fuzzGameOver "Remote requests rematch and disconnects" <|
        update RemoteRequestedRematch
            >> Tuple.first
            >> update RemoteDisconnected
            >> expectNoOpponent RemoteForfeitAlert
    , fuzzGameOver "Decline remote-requested rematch" <|
        update RemoteRequestedRematch
            >> Tuple.first
            >> update ReturnToLobby
            >> expectGameOverState ReturningToLobby
    , fuzzGameOver "Decline remote-requested rematch and disconnect" <|
        update RemoteRequestedRematch
            >> Tuple.first
            >> update ReturnToMenu
            >> expectGameOverState ReturningToMenu
    ]
    |> describe "Rematch Logic"











