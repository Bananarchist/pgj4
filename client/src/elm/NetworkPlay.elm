module NetworkPlay exposing (..)
import ConnectionData exposing (ConnectionData)
import BattleLogic as BL exposing (BattleState, BattleAction)
import Html exposing (Html)
import Html.Attributes as Hats
import Html.Events as Emit
import Html exposing (b)
import U


type Host 
    = Host String
    | RemoteHost String

isHost : Host -> Bool
isHost host =
    case host of
        Host _ ->
            True
        _ ->
            False

type GameOverState
    = RematchRequested
    | RematchDeclined
    | RemoteRematchRequested
    | Rematch
    | ReviewingResults
    | ReturningToLobby
    | ReturningToMenu

type DisconnectionState
    = DisconnectionAlert
    | DisconnectToMenu

type RemoteForfeitState
    = RemoteForfeitAlert
    | RemoteDeclinedRematch
    | RemoteForfeitToMenu
    | RemoteForfeitToLobby

type ForfeitState
    = ForfeitToLobby
    | ForfeitToMenu

type Model 
    = Playing Host BattleState ConnectionData
    | Disconnected DisconnectionState
    | NoOpponent RemoteForfeitState ConnectionData
    | Forfeit ForfeitState
    | GameOver GameOverState Host BattleState ConnectionData

type Msg
    = RemoteAction BattleAction
    | ClientAction BattleAction
    | RemoteDisconnected
    | RemoteRequestedRematch
    | Disconnect
    | ReturnToLobby
    | ReturnToMenu
    | RequestRematch

init : Host -> BattleState -> ConnectionData -> Model
init =
    Playing

concludeGame : Model -> Model
concludeGame model =
    case model of
        Playing host (BL.Player1Victory h) connectionData ->
            GameOver ReviewingResults host (BL.Player1Victory h) connectionData
        Playing host (BL.Player2Victory h) connectionData ->
            GameOver ReviewingResults host (BL.Player2Victory h) connectionData
        _ ->
            model

returningToLobbyか : Model -> Bool
returningToLobbyか model =
    case model of
        GameOver ReturningToLobby _ _ _ ->
            True
        NoOpponent RemoteForfeitToLobby _ ->
            True
        Forfeit ForfeitToLobby ->
            True
        _ ->
            False

returningToMenuか : Model -> Bool
returningToMenuか model =
    case model of
        GameOver ReturningToMenu _ _ _ ->
            True
        NoOpponent RemoteForfeitToMenu _ ->
            True
        Forfeit ForfeitToMenu ->
            True
        _ ->
            False

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case (model, msg) of
        (Playing host battleState connectionData, RemoteAction action) ->
            ( Playing host (BL.update action battleState) connectionData
                |> concludeGame
            , Cmd.none
            )
        (Playing host battleState connectionData, ClientAction action) ->
            ( Playing host (BL.update action battleState) connectionData
                |> concludeGame
            , Cmd.none
            )
        (Playing host battleState connectionData, Disconnect) ->
            ( Disconnected DisconnectionAlert
            , Cmd.none
            )
        (Playing host battleState connectionData, RemoteDisconnected) ->
            ( NoOpponent RemoteForfeitAlert connectionData
            , Cmd.none
            )
        (Disconnected DisconnectionAlert, ReturnToMenu) ->
            ( Disconnected DisconnectToMenu
            , Cmd.none
            )
        (NoOpponent RemoteForfeitAlert connectionData, ReturnToMenu) ->
            ( NoOpponent RemoteForfeitToMenu connectionData
            , Cmd.none
            )
        (NoOpponent RemoteForfeitAlert connectionData, ReturnToLobby) ->
            ( NoOpponent RemoteForfeitToLobby connectionData
            , Cmd.none
            )
        (GameOver ReviewingResults host battleState connectionData, RequestRematch) ->
            ( GameOver RematchRequested host battleState connectionData
            , Cmd.none
            )
        (GameOver RematchRequested host battleState connectionData, RemoteDisconnected) ->
            ( NoOpponent RemoteDeclinedRematch connectionData
            , Cmd.none
            )
        (GameOver _ host battleState connectionData, RemoteDisconnected) ->
            ( NoOpponent RemoteForfeitAlert connectionData
            , Cmd.none
            )
        (GameOver ReviewingResults host battleState connectionData, RemoteRequestedRematch) ->
            ( GameOver RemoteRematchRequested host battleState connectionData
            , Cmd.none
            )
        (GameOver RematchRequested host battleState connectionData, RemoteRequestedRematch) ->
            ( GameOver Rematch host battleState connectionData
            , Cmd.none
            )
        (GameOver _ host battleState connectionData, ReturnToLobby) ->
            ( GameOver ReturningToLobby host battleState connectionData
            , Cmd.none
            )
        (GameOver _ host battleState connectionData, ReturnToMenu) ->
            ( GameOver ReturningToMenu host battleState connectionData
            , Cmd.none
            )
        _ -> 
            ( model
            , Cmd.none
            )

subscriptions : Model -> Sub Msg
subscriptions = always Sub.none

view : Model -> List (Html Msg)
view model =
    case model of
        Playing host battleState connectionData ->
            viewBattle host battleState connectionData
        Disconnected DisconnectionAlert ->
            viewDisconnected
        NoOpponent RemoteForfeitAlert connectionData ->
            viewRemoteForfeitAlert connectionData
        NoOpponent RemoteDeclinedRematch connectionData ->
            viewRemoteDeclinedRematch connectionData
        GameOver ReviewingResults host battleState connectionData ->
            viewResults host battleState connectionData
            ++ viewMatchResults host battleState connectionData
        GameOver RematchRequested host battleState connectionData ->
            viewResults host battleState connectionData
            ++ viewRematchRequested host battleState connectionData
        GameOver RemoteRematchRequested host battleState connectionData ->
            viewResults host battleState connectionData
            ++ viewRemoteRematchRequested host battleState connectionData
        _ ->
            []

viewBattle : Host -> BattleState -> ConnectionData -> List (Html Msg)
viewBattle host battleState connectionData =
    [ Html.div [] [ Html.text "Battle" ]
    , Html.div [] 
        (List.concatMap (\fn -> fn host battleState connectionData)
            [ viewBattleSummary --host battleState connectionData 
            --, viewBattleHistory host battleState connectionData
            , viewOpponentArmy --host battleState connectionData
            , viewPlayerArmy --host battleState connectionData
            ])
    , Html.button [ Emit.onClick (ClientAction BL.Forfeit) ] [ Html.text "Forfeit" ]
    ]

viewOpponentArmy : Host -> BattleState -> ConnectionData -> List (Html Msg)
viewOpponentArmy host battleState connectionData =
    let
        opponentArmy = 
            if isHost host then
                BL.player2Army battleState
            else
                BL.player1Army battleState
    in
    [ Html.div [] 
        ( opponentArmy 
            |> BL.armyPiecesList
            |> List.indexedMap viewPiece 
            |> List.concatMap identity
        )
    ]

viewPlayerArmy : Host -> BattleState -> ConnectionData -> List (Html Msg)
viewPlayerArmy host battleState connectionData =
    let
        playerArmy = 
            if isHost host then
                BL.player1Army battleState
            else
                BL.player2Army battleState
    in
    [ Html.div [] 
        ( playerArmy 
            |> BL.armyPiecesList
            |> List.indexedMap 
                (\idx unit -> 
                    if BL.player1Turnか battleState then 
                        viewControllablePiece idx unit 
                    else 
                        viewPiece idx unit
                )
            |> List.concatMap identity
        )
    ]

viewPiece : Int -> BL.Piece -> List (Html Msg)
viewPiece idx piece =
    case piece of
        BL.Obliterated ->
            [ idx |> String.fromInt
            , "undefended"
            ]
                |> String.join " "
                |> U.singletonText
                |> Html.h3 []
                |> List.singleton
        BL.Piece hue honesty ->
                [ String.fromInt idx
                , hue
                    |> (
                        if BL.authenticか piece then 
                            BL.hueString
                        else 
                            BL.oppositeHue >> BL.hueString >> (++) "Fake "
                       )
                ]
                |> String.join " "
                |> U.singletonText
                |> Html.h3 []
                |> List.singleton

viewControllablePiece : Int -> BL.Piece -> List (Html Msg)
viewControllablePiece idx piece =
    viewPiece idx piece
    |> case piece of
        BL.Obliterated ->
            identity
        BL.Piece hue honesty ->
            \p ->
                p
                ++
                [ Html.div [] 
                    (Html.button [ Emit.onClick (ClientAction (BL.Fire idx)) ] [ Html.text "Attack" ]
                    :: viewMoveButtons idx)
                ]


viewMoveButtons : Int -> List (Html Msg)
viewMoveButtons idx =
    List.range 1 5
    |> List.foldr (\i acc -> 
        ( i 
            |> String.fromInt 
            |> U.singletonText 
            |> Html.button [ Hats.disabled (i == idx), Emit.onClick (ClientAction (BL.Switch idx i)) ]
        ) :: acc
    ) []

viewBattleSummary : Host -> BattleState -> ConnectionData -> List (Html Msg)
viewBattleSummary host battleState connectionData =
    let

        turnString = 
            case (BL.player1Turnか battleState, host) of
                (True, _) ->
                    "Your Turn"
                (False, Host n) ->
                    n ++ "'s turn"
                (False, RemoteHost n) ->
                    n ++ "'s turn"
        (currentHealth, opponentHealth) = 
            U.mapTuple String.fromInt (
                if isHost host then
                    (BL.player1Health battleState, BL.player2Health battleState)
                else
                    (BL.player2Health battleState, BL.player1Health battleState)
            )

    in
    [ Html.div [] 
        [ Html.div [] [ Html.text "Battle Summary" ]
        , Html.div [] 
            [ Html.div [] [ Html.text "Turn: ", Html.text turnString ]
            , Html.div [] [ Html.text "Player: ", Html.text currentHealth ]
            , Html.div [] [ Html.text "Opponent: ", Html.text opponentHealth ]
            ]
        ]
    ]

viewDisconnected : List (Html Msg)
viewDisconnected =
    [ Html.text "Connection lost" 
    , Html.button [ Emit.onClick ReturnToMenu ] [ Html.text "Return to Menu" ]
    ]

viewMatchResults : Host -> BattleState -> ConnectionData -> List (Html Msg)
viewMatchResults host bs connectionData =
    [ Html.button [ Emit.onClick RequestRematch ] [ Html.text "Request Rematch" ]
    , Html.button [ Emit.onClick ReturnToLobby ] [ Html.text "Return to Lobby" ]
    , Html.button [ Emit.onClick ReturnToMenu ] [ Html.text "Return to Menu" ]
    ]


viewResults : Host -> BattleState -> ConnectionData -> List (Html Msg)
viewResults host bs _ =
    case (host, bs) of
        (Host _, BL.Player1Victory _) ->
            [ Html.p [] [ Html.text "Congratulations ", Html.i [] [ Html.text "WINNER" ] ]
            ]
        (Host n, BL.Player2Victory _) ->
            [ Html.p [] [ Html.text (n ++ " wins!")]
            ]
        (RemoteHost n, BL.Player1Victory _) ->
            [ Html.p [] [ Html.text (n ++ " wins!")]
            ]
        (RemoteHost _, BL.Player2Victory _) ->
            [ Html.p [] [ Html.text "Congratulations ", Html.i [] [ Html.text "WINNER" ] ]
            ]
        _ -> []


viewRematchRequested : Host -> BattleState -> ConnectionData -> List (Html Msg)
viewRematchRequested host bs connectionData =
    [ Html.p [] [ Html.text "Waiting for opponent to accept rematch..." ]
    , Html.button [ Emit.onClick ReturnToLobby ] [ Html.text "Return to Lobby" ]
    , Html.button [ Emit.onClick ReturnToMenu ] [ Html.text "Return to Menu" ]
    ]

viewRemoteRematchRequested : Host -> BattleState -> ConnectionData -> List (Html Msg)
viewRemoteRematchRequested host bs connectionData =
    [ Html.p [] [ Html.text "Opponent has requested a rematch." ]
    , Html.button [ Emit.onClick RequestRematch ] [ Html.text "Accept Rematch" ]
    , Html.button [ Emit.onClick ReturnToLobby ] [ Html.text "Return to Lobby" ]
    , Html.button [ Emit.onClick ReturnToMenu ] [ Html.text "Return to Menu" ]
    ]


viewRemoteDeclinedRematch : ConnectionData -> List (Html Msg)
viewRemoteDeclinedRematch connectionData =
    [ Html.p [] [ Html.text "Opponent has declined rematch." ]
    , Html.button [ Emit.onClick ReturnToLobby ] [ Html.text "Return to Lobby" ]
    , Html.button [ Emit.onClick ReturnToMenu ] [ Html.text "Return to Menu" ]
    ]

viewRemoteForfeitAlert : ConnectionData -> List (Html Msg)
viewRemoteForfeitAlert connectionData =
    [ Html.p [] [ Html.text "Opponent has forfat." ]
    , Html.button [ Emit.onClick ReturnToLobby ] [ Html.text "Return to Lobby" ]
    , Html.button [ Emit.onClick ReturnToMenu ] [ Html.text "Return to Menu" ]
    ]









