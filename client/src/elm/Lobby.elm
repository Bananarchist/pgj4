module Lobby exposing (..)

import Server
import ConnectionData exposing (ConnectionData)
import Ports
import Html exposing (Html)
import Html.Attributes as Hats
import Html.Events as Emit
import U

type Model
    = Lobby LobbyData
    | EnteringLobby ConnectionData
    | Disconnected ConnectionData
    | Reconnecting ConnectionData
    | ReturningToNetworkMenu
    | ReturningToMenu

type alias LobbyData =
    { connectionData : ConnectionData
    , userList : List Server.User
    , gameList : List Server.Game
    , chatMsgs : List ChatMsg
    , chatBox : String
    }

type alias ChatMsg =
    { senderId : String
    , message : String
    , timestamp : String
    }

type NetworkStatusMsg
    = ConnectionClosed
    | ConnectionOpened

type UIMsg
    = InputChat String
    | SendChat

type FlowMsg
    = ToMenu
    | ToNetworkMenu
    | Reconnect

type Msg
    = Network NetworkStatusMsg
    | Flow FlowMsg
    | ServerMessage Server.Msg
    | UI UIMsg


mapConnectionData : (ConnectionData -> ConnectionData) -> Model -> Model
mapConnectionData f model =
    case model of
        Lobby lobbyData ->
            Lobby { lobbyData | connectionData = f lobbyData.connectionData }

        EnteringLobby cdata ->
            EnteringLobby (f cdata)

        Disconnected cdata ->
            Disconnected (f cdata)

        Reconnecting cdata ->
            Reconnecting (f cdata)

        _ -> model

setUserList : List Server.User -> Model -> Model
setUserList users model =
    case model of
        Lobby lobbyData ->
            Lobby { lobbyData | userList = users }

        _ -> model

setGameList : List Server.Game -> Model -> Model
setGameList games model =
    case model of
        Lobby lobbyData ->
            Lobby { lobbyData | gameList = games }

        _ -> model

setChatMsgs : List ChatMsg -> Model -> Model
setChatMsgs msgs model =
    case model of
        Lobby lobbyData ->
            Lobby { lobbyData | chatMsgs = msgs }

        _ -> model

mapChatMsgs : (List ChatMsg -> List ChatMsg) -> Model -> Model
mapChatMsgs f model = 
    case model of
        Lobby lobbyData ->
            Lobby { lobbyData | chatMsgs = f lobbyData.chatMsgs }

        _ -> model

init : ConnectionData -> (Model, Cmd Msg)
init cdata =
    (EnteringLobby cdata, Ports.send (Server.setName cdata.username |> Server.encode))

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case (model, msg) of
        (EnteringLobby cdata, ServerMessage (Server.SetName newName)) ->
            (Lobby { connectionData = { cdata | username = newName }, userList = [], gameList = [], chatMsgs = [], chatBox = "" }, Cmd.none)

        (Lobby lobbyData, Network networkMsg) ->
            case networkMsg of
                ConnectionClosed ->
                    (Disconnected lobbyData.connectionData, Cmd.none)

                ConnectionOpened ->
                    (Lobby lobbyData, Ports.send (Server.setName lobbyData.connectionData.username |> Server.encode))

        (Lobby lobbyData, ServerMessage serverMsg) ->
            case serverMsg of
                Server.UserList users ->
                    (setUserList users model, Cmd.none )

                Server.GameList games ->
                    (setGameList games model, Cmd.none)

                Server.Chat senderId message timestamp ->
                    (mapChatMsgs (\cm -> { senderId = senderId, message = message, timestamp = timestamp } :: cm) model, Cmd.none)

                _ -> (model, Cmd.none)

        (Lobby lobbyData, UI uiMsg) ->
            case uiMsg of
                InputChat input ->
                    -- should check if enter to send chat
                    (Lobby { lobbyData | chatBox = input }, Cmd.none)

                SendChat ->
                    (Lobby { lobbyData | chatBox = "" }, Ports.send (Server.sendChat lobbyData.chatBox |> Server.encode))

        (Lobby lobbyData, Flow flowMsg) ->
            case flowMsg of
                ToMenu ->
                    (ReturningToMenu, Cmd.none)

                ToNetworkMenu ->
                    (ReturningToNetworkMenu, Cmd.none)

                Reconnect ->
                    (Reconnecting lobbyData.connectionData, Ports.send (Server.setName lobbyData.connectionData.username |> Server.encode))

        _ -> (model, Cmd.none)


websocketOpenHandler : String -> Msg
websocketOpenHandler =
    always (Network ConnectionOpened)

websocketClosedHandler : String -> Msg
websocketClosedHandler =
    always (Network ConnectionClosed)

websocketReceiver : String -> Msg
websocketReceiver =
    Server.decode >> ServerMessage


{-| view function for Lobby, showing simple views for connection and error states and
a three-pane view for the lobby itself with chat window, gamelist window, userlist window
and control panel -}
view : Model -> List (Html Msg)
view model =
    case model of
        EnteringLobby cdata ->
            viewEnteringLobby cdata

        Disconnected cdata ->
            viewDisconnected cdata

        Reconnecting cdata ->
            viewReconnecting cdata

        Lobby lobbyData ->
            viewLobby lobbyData

        _ -> []

viewLobby : LobbyData -> List (Html Msg)
viewLobby lobbyData =
    List.foldl ((|>) lobbyData >> (++)) [] [ viewChat, viewGameList, viewUserList, viewControlPanel ]


viewControlPanel : LobbyData -> List (Html Msg)
viewControlPanel lobbyData =
    [ Html.div 
        [ Hats.id "controlpanel" ]
        [ Html.button 
            [ Hats.class "returntomenu"
            , Emit.onClick (Flow ToMenu)
            ] 
            [ Html.text "Return to Menu" ]
        , Html.button 
            [ Hats.class "returntonetworkmenu"
            , Emit.onClick (Flow ToNetworkMenu)
            ] 
            [ Html.text "Return to Network Menu" ]
        ]
    ]

viewUserList : LobbyData -> List (Html Msg)
viewUserList lobbyData =
    [ Html.div [ Hats.id "userlist" ]
        (List.map viewUser lobbyData.userList)
    ]

viewUser : Server.User -> Html Msg
viewUser user =
    Html.div [ Hats.class "user" ] [ Html.text user.username ]

viewGameList : LobbyData -> List (Html Msg)
viewGameList lobbyData =
    [ Html.div [ Hats.id "gamelist" ]
        (List.concatMap viewGame lobbyData.gameList)
    ]

viewGame : Server.Game -> List (Html Msg)
viewGame game =
    [ Html.div [ Hats.class "game" ] [ Html.text game ]
    , Html.button [ Hats.class "joingame" ] [ Html.text "Join" ]
    ]

viewChat : LobbyData -> List (Html Msg)
viewChat lobbyData =
    [ Html.div [ Hats.class "chat" ]
        (List.map (viewChatMsg lobbyData.userList) lobbyData.chatMsgs)
    , Html.fieldset [ Hats.id "chatinputfieldset" ]
        [ Html.input 
            [ Hats.id "chatinput"
            , Hats.value lobbyData.chatBox
            , Emit.onInput (UI << InputChat) 
            ] 
            [ ]
        , Html.button
            [ Hats.id "chatsend"
            , Emit.onClick (UI SendChat) 
            ] 
            [ Html.text "Send" ]
        ]
    ]

viewChatMsg : List Server.User -> ChatMsg -> Html Msg
viewChatMsg users chatMsg =
    let
        senderName =
            users 
            |> List.filter (\u -> u.userId == chatMsg.senderId) 
            |> List.map .username 
            |> List.head 
            |> Maybe.withDefault "Anon"
    in
        Html.p [ Hats.class "chatmsg" ]
            [ Html.div [ Hats.class "chatmsg-sender" ] [ Html.text senderName ]
            , Html.div [ Hats.class "chatmsg-message" ] [ Html.text chatMsg.message ]
            , Html.div [ Hats.class "chatmsg-timestamp" ] [ Html.text chatMsg.timestamp ]
            ]



viewEnteringLobby : ConnectionData -> List (Html Msg)
viewEnteringLobby cdata =
    [ Html.h1 [] [ Html.text "Connection established" ]
    , Html.p [] [ Html.text "Waiting for data..." ]
    ]


viewDisconnected : ConnectionData -> List (Html Msg)
viewDisconnected cdata =
    [ Html.h1 [] [ Html.text "Disconnected from network" ]
    , Html.button [ Emit.onClick (Flow Reconnect) ] [ cdata |> ConnectionData.connectionString |> (++) "Reconnect " |> Html.text ]
    , Html.button [ Emit.onClick (Flow ToNetworkMenu) ] [ Html.text "Connect to different network" ]
    , Html.button [ Emit.onClick (Flow ToMenu) ] [ Html.text "Return to main menu" ]
    ]


viewReconnecting : ConnectionData -> List (Html Msg)
viewReconnecting cdata =
    [ Html.h1 [] [ Html.text "Reconnecting to network..." ]
    , Html.p [] [ cdata |> ConnectionData.connectionString |> Html.text ]
    , Html.button [ Emit.onClick (Flow ToNetworkMenu) ] [ Html.text "Connect to different network" ]
    , Html.button [ Emit.onClick (Flow ToMenu) ] [ Html.text "Return to main menu" ]
    ]



    
