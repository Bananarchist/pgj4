module LobbyTests exposing (suite)

import Expect exposing (Expectation)
import Lobby exposing (..)
import Test exposing (..)
import Fuzz exposing (Fuzzer)
import Fuzz.Extra exposing (connectionData)
import BattleLogic as BL
import Server

suite : Test
suite =
    [ lobbyEntranceTests
    ]
    |> describe "Lobby"

expectLobbyState : (Model, Cmd Msg) -> Expectation
expectLobbyState (model, _) =
    case model of
        Lobby _  -> Expect.pass
        _ -> 
            String.join "\n"
                [ "Expected lobby state, but got:"
                , Debug.toString model
                ]
            |> Expect.fail


lobbyEntranceTests : Test
lobbyEntranceTests =
    [ fuzz connectionData "should return a LobbyEntrance message" <|
        Lobby.init
        >> Tuple.first
        >> update (ServerMessage (Server.SetName "test"))
        >> expectLobbyState
    ]
    |> describe "lobbyEntrance"
