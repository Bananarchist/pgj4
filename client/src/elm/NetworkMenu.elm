module NetworkMenu exposing (..)

import ConnectionData
import Ports
import ConnectionData exposing (ConnectionData)
import Json.Decode as D
import Html exposing (Html)
import Html.Attributes as Hats
import Html.Events as Emit
import LocalStorage exposing (LocalStorage)

type alias Reason = String
type alias Port = Int
type alias Name = String
type alias IP = String

type FieldValidity t
    = Valid t
    | Invalid t Reason
    | Untouched

type Model
    = NameDefinition (FieldValidity Name)
    | HostDefinition (FieldValidity IP) (FieldValidity Port) Name
    | AttemptingConnection ConnectionData
    | ResolvingRejectedName (FieldValidity Name) IP Port
    | ResolvingConnectionFailure (FieldValidity IP) (FieldValidity Port) Name


type Msg
    = NameUpdated Name
    | NameFinished
    | ReturnToNameEntry
    | IPUpdated IP
    | PortUpdated Port
    | ConnectTo ConnectionData
    | AttemptConnection
    | NameNotAccepted Reason
    | ConnectionFailed Reason
    | Success ConnectionData
    | ReturnToMainMenu

attemptToConnect = Ports.openWebSocket

defaultPort : Int
defaultPort = 9160

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case (model, msg) of 
        (NameDefinition fName, NameUpdated newName) -> (NameDefinition (nameValidity newName), Cmd.none)
        (NameDefinition (Valid name), NameFinished) -> (HostDefinition Untouched (Valid defaultPort) name, Cmd.none)
        (HostDefinition _ _ name, ReturnToNameEntry) -> (NameDefinition (nameValidity name), Cmd.none)
        (HostDefinition fIp fPort name, IPUpdated ip) -> (HostDefinition (ipValidity ip) fPort name, Cmd.none)
        (HostDefinition fIp fPort name, PortUpdated port_) -> (HostDefinition fIp (portValidity port_) name, Cmd.none)
        (HostDefinition (Valid ip) (Valid port_) name, AttemptConnection) ->
            let cdata = ConnectionData.init ip port_ name in
            (AttemptingConnection cdata, attemptToConnect cdata)
        (NameDefinition _, ConnectTo cdata) ->
            (AttemptingConnection cdata, attemptToConnect cdata)
        (AttemptingConnection {username,host,port_}, NameNotAccepted reason) ->
            (ResolvingRejectedName (Invalid username reason) host port_, Cmd.none)
        (AttemptingConnection {username,host,port_}, ConnectionFailed reason) ->
            (ResolvingConnectionFailure (Invalid host reason) (Invalid port_ reason) username, Cmd.none)
        (ResolvingRejectedName name ip prt, NameUpdated newName) ->
            (ResolvingRejectedName (nameValidity newName) ip prt, Cmd.none)
        (ResolvingRejectedName (Valid name) ip port_, NameFinished) ->
            let cdata = ConnectionData.init ip port_ name in
            (AttemptingConnection cdata, attemptToConnect cdata)
        (ResolvingConnectionFailure ip port_ name, IPUpdated newIp) ->
            (ResolvingConnectionFailure (ipValidity newIp) port_ name, Cmd.none)
        (ResolvingConnectionFailure ip port_ name, PortUpdated newPort) ->
            (ResolvingConnectionFailure ip (portValidity newPort) name, Cmd.none)
        (ResolvingConnectionFailure (Valid ip) (Valid port_) name, AttemptConnection) ->
            let cdata = ConnectionData.init ip port_ name in
            (AttemptingConnection cdata, attemptToConnect cdata)
        _ -> (model, Cmd.none)

ipValid : IP -> Bool
ipValid ip =
    True

ipValidity : IP -> FieldValidity IP
ipValidity ip =
    if ipValid ip then
        Valid ip
    else
        Invalid ip ""

portValid : Port -> Bool
portValid port_ =
    True

portValidity : Port -> FieldValidity Port
portValidity port_ =
    if portValid port_ then
        Valid port_
    else
        Invalid port_ ""



nameValid : Name -> Bool
nameValid = always True

nameValidity : Name -> FieldValidity Name
nameValidity name =
    if nameValid name then
        Valid name
    else
        Invalid name ""

socketOpenedHandler : Model -> String -> Msg
socketOpenedHandler model msg =
    case model of
        AttemptingConnection cdata -> Success cdata
        _ -> ConnectionFailed "Could not orchestrate state"

socketClosedHandler : Model -> String -> Msg
socketClosedHandler model msg =
    ConnectionFailed "Could not connect! Perhaps IP/Port were wrong?"


view : LocalStorage -> Model -> List (Html Msg)
view localStorage nmod =
    let
       priorNames = (List.map .username localStorage.connectionData)
       priorIpsAndPorts forName = 
           List.filter (.username >> (==) forName) localStorage.connectionData 
           |> List.map (\{host,port_} -> (host, port_))

    in
    case nmod of
        NameDefinition fName ->
            [ nameEntry priorNames fName
            , finishNameEntryButton fName
            , networkMenuToMainMenuButton
            ]
            ++ 
            ( List.map viewPastConnection localStorage.connectionData
            )
        HostDefinition fIP fPort name ->
            [ Html.p [] [ Html.text "Connecting as ", Html.text name ]
            , ipEntry (priorIpsAndPorts name) fIP
            , portEntry (priorIpsAndPorts name) fPort
            , finishHostEntryButton fIP fPort
            , reenterNameButton
            , networkMenuToMainMenuButton
            ]
        AttemptingConnection cdata ->
            [ Html.p [] [ Html.text "Attempting to connect to ", Html.text cdata.host, Html.text ":", Html.text (String.fromInt cdata.port_) ] ]
        ResolvingRejectedName fName ip port_ ->
            [ nameEntry priorNames fName
            , finishNameEntryButton fName
            , networkMenuToMainMenuButton
            ]
        ResolvingConnectionFailure fIP fPort name ->
            [ Html.p [] [ Html.text "Connecting as ", Html.text name ]
            , ipEntry (priorIpsAndPorts name) fIP
            , portEntry (priorIpsAndPorts name) fPort
            , finishHostEntryButton fIP fPort
            , reenterNameButton
            , networkMenuToMainMenuButton
            ]

viewPastConnection : ConnectionData -> Html Msg
viewPastConnection ({username,host,port_} as cdata) =
    Html.button [ Emit.onClick (ConnectTo cdata) ] [ Html.text username, Html.text "@", Html.text host, Html.text ":", Html.text (String.fromInt port_) ]

reenterNameButton : Html Msg
reenterNameButton = Html.button [ Emit.onClick ReturnToNameEntry ] [ Html.text "Back" ]

finishHostEntryButton : FieldValidity IP -> FieldValidity Port -> Html Msg
finishHostEntryButton fIP fPort =
    let
        disabled = 
            case (fIP, fPort) of
                (Valid _, Valid _) -> False
                _ -> True
    in
    Html.button [ Hats.disabled disabled, Emit.onClick AttemptConnection ] [ Html.text "Connect" ]

ipEntry : List (String, Int) -> FieldValidity IP -> Html Msg
ipEntry priorIpsAndPorts fIp =
    let
        (ipValue, errors) =
            case fIp of
                Valid ip -> (ip, [])
                Untouched -> ("", [])
                Invalid ip reason -> (ip, [Html.p [] [ Html.text reason ]])
    in
    [ Html.label 
        [ Hats.for "ip-input" ] 
        [ Html.text "IP" ]
    , Html.input 
        [ Hats.id "ip-input" 
        , Hats.list "ip-datalist"
        , Hats.value ipValue 
        , Emit.onInput IPUpdated
        ]
        []
    , Html.datalist 
        [ Hats.id "ip-datalist" ] 
        (List.map (\(ip, _) -> Html.option [ Hats.value ip ] []) priorIpsAndPorts)
    ]
    ++ errors
    |> Html.fieldset [] 

portEntry : List (String, Int) -> FieldValidity Port -> Html Msg
portEntry priorIpsAndPorts fPort =
    let
        (portValue, errors) =
            case fPort of
                Valid port_ -> (port_, [])
                Untouched -> (defaultPort, [])
                Invalid port_ reason -> (port_, [Html.p [] [ Html.text reason ]])
    in
    [ Html.label 
        [ Hats.for "port-input" ] 
        [ Html.text "Port" ]
    , Html.input 
        [ Hats.id "port-input" 
        , Hats.list "port-datalist"
        , Hats.value (String.fromInt portValue)
        , Emit.onInput (String.toInt >> Maybe.withDefault defaultPort >> PortUpdated)
        ]
        []
    , Html.datalist 
        [ Hats.id "port-datalist" ] 
        (List.map (\(_, port_) -> Html.option [ Hats.value (String.fromInt port_) ] []) priorIpsAndPorts)
    ]
    ++ errors
    |> Html.fieldset [] 



nameEntry : List String -> FieldValidity Name -> Html Msg
nameEntry priorNames fName =
    let
        (nameValue, errors) =
            case fName of
                Valid name -> (name, [])
                Untouched -> ("", [])
                Invalid name reason -> (name, [Html.p [] [ Html.text reason ]])
    in
    [ Html.label 
        [ Hats.for "username-input" ] 
        [ Html.text "Username" ]
    , Html.input 
        [ Hats.id "username-input" 
        , Hats.list "username-datalist"
        , Hats.value nameValue 
        , Emit.onInput NameUpdated
        ]
        []
    , Html.datalist 
        [ Hats.id "username-datalist" ] 
        (List.map (\name -> Html.option [ Hats.value name ] []) priorNames)
    ]
    ++ errors
    |> Html.fieldset [] 

finishNameEntryButton : FieldValidity Name -> Html Msg
finishNameEntryButton fName = 
    let
        disabled =
            case fName of
                Valid _ -> False
                _ -> True
    in
    Html.button [ Hats.disabled disabled, Emit.onClick NameFinished ] [ Html.text "Confirm" ]
                    
networkMenuToMainMenuButton : Html Msg
networkMenuToMainMenuButton =
    Html.button [ Emit.onClick ReturnToMainMenu ] [ Html.text "Main Menu" ]
    
