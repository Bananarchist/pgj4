module Main exposing (main)

import Html exposing (Html)
import Html.Attributes as Hats
import Html.Events as Emit
import Basics.Extra exposing (flip)
import BattleLogic
import LowGFX
import NoGFX
import GraphicsMode as GM
import Msg exposing (Msg(..))
import MainMenu as MM
import Play as P
import LocalStorage as LS
import PlayerSetup as PS
import ConnectionData as CD
import NetworkMenu as NM
import Settings as S
import SettingsMenu as SM
import Dict exposing (Dict)
import Lobby as LB
import Browser
import PiecePlacement
import Ports
import U


main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }


type alias App =
    { appState : AppState
    , storedData : LS.LocalStorage
    }


type AppState
    = Initialization
    | MainMenu
    | Configuration
    | Credits
    | ConnectingToNetwork NM.Model
    | Lobby LB.Model
    | PlayerSetup AppState PS.Model
    | Play P.Model
    --| PieceSetting (Maybe SameScreenSetup)

init : () -> ( App, Cmd Msg )
init _ =
    --( App initSettings (Play (P.litSameScreen ("HORK", BattleLogic.blueArmy) ("Flan", BattleLogic.orangeArmy))), Cmd.none)
    ( App MainMenu LS.init, Ports.getLocalStorageKeys )


subscriptions : App -> Sub Msg
subscriptions app =
    let 
        appStateSubs = 
            case app.appState of
                Initialization -> Sub.none
                MainMenu -> Sub.none
                Configuration -> Sub.none
                Credits -> Sub.none
                ConnectingToNetwork ((NM.AttemptingConnection _) as nmod) ->
                    Sub.map NetworkMenuMsg 
                        (Sub.batch
                            [ Ports.socketOpened (NM.socketOpenedHandler nmod)
                            , Ports.socketClosed (NM.socketClosedHandler nmod)
                            ]
                        )
                        
                Lobby _ ->
                    Sub.map LobbyMsg
                        (Sub.batch
                            [ (Ports.socketOpened LB.websocketOpenHandler)
                            , (Ports.socketClosed LB.websocketClosedHandler)
                            , (Ports.socketMessageReceiver LB.websocketReceiver)
                            ]
                        )
                PlayerSetup _ _ -> Sub.none
                --Play _ _ -> Sub.none
                _ -> Sub.none
    in
    [ appStateSubs, Ports.localStorageListener LocalStorageRetrieved ]
    |> Sub.batch



swapPlayers = identity

update : Msg -> App -> ( App, Cmd Msg )
update msg ({appState, storedData} as app) =
    case (appState, msg) of
        (_, LocalStorageRetrieved localStorage ) ->
            ( { app | storedData = localStorage }
            , Cmd.none
            )
        (MainMenu, MainMenuMsg mevt) ->
            case mevt of
                MM.StartLocalGame ->
                    ({ app | appState = PlayerSetup app.appState PS.initSameScreen }, Cmd.none)
                MM.ConnectToNetwork ->
                    (initializeNetworkConnections app, Cmd.none)
                MM.AdjustSettings ->
                    ({ app | appState = Configuration }, Cmd.none)
                _ ->
                    ( app, Cmd.none )
        (ConnectingToNetwork nmod, NetworkMenuMsg nevt) ->
            case nevt of
                NM.Success cdata ->
                    let
                        (_, ncmds) = NM.update nevt nmod
                        (lmod, lcmds) = LB.init cdata
                        -- needs to check it doesn't already exist...
                        newLocalStorage = LS.addConnectionDataEntry cdata storedData
                    in
                    ( { app | appState = Lobby lmod, storedData = newLocalStorage}
                    , [ Cmd.map NetworkMenuMsg ncmds
                      , Cmd.map LobbyMsg lcmds
                      , Ports.saveLocalStorageCmd newLocalStorage
                      ]
                        |> Cmd.batch
                    )
                _ ->
                    NM.update nevt nmod
                    |> Tuple.mapBoth (\newNmod -> { app | appState = ConnectingToNetwork newNmod }) (Cmd.map NetworkMenuMsg)

        (Lobby lmod, LobbyMsg levt) ->
            let
                newState = LB.update levt lmod 
            in
            case Tuple.first newState of
                LB.ReturningToMenu ->
                    ({ app | appState = MainMenu }, Cmd.none)
                LB.ReturningToNetworkMenu ->
                    (initializeNetworkConnections app, Cmd.none)
                _ ->
                    newState |> Tuple.mapFirst (Lobby >> flip setAppState app) |> Tuple.mapSecond (Cmd.map LobbyMsg)

        (PlayerSetup lastState psModel, PlayerSetupMsg psm) ->
            let 
                (newPsModel, cmds) = PS.update psm psModel
            in
            case newPsModel of
                PS.StartingSameScreenGame p1 p2 ->
                    ({ app | appState = Play (P.initSameScreen p1 p2) }, Cmd.map Msg.PlayerSetupMsg cmds)
                PS.ReturningToMenu ->
                    ({ app | appState = lastState }, Cmd.map Msg.PlayerSetupMsg cmds)
                _ ->
                    ({ app | appState = PlayerSetup lastState newPsModel }, Cmd.map Msg.PlayerSetupMsg cmds)

        (Configuration, ReturnToMenu) ->
            ({ app | appState = MainMenu }, Cmd.none)
            {-
        (Configuration, ChangeGraphicsMode gmode) ->
            Ports.setGraphicsMode gmode app.storedData
            |> Tuple.mapFirst (\s -> { app | storedData = s})
        (Configuration, EnableAnimations) ->
            Ports.setAnimations True app.storedData
            |> Tuple.mapFirst (\s -> { app | storedData = s})
        (Configuration, DisableAnimations) ->
            Ports.setAnimations False app.storedData
            |> Tuple.mapFirst (\s -> { app | storedData = s})
            -}
        (Play pm, PlayMsg pmsg) ->
            P.update pmsg pm
            |> Tuple.mapBoth (\nm -> {app | appState = Play nm }) (Cmd.map PlayMsg)
        _ ->
            (app, Cmd.none)

setAppState : AppState -> App -> App
setAppState atate app = { app | appState = atate }

initializeSettingsMenu : App -> App
initializeSettingsMenu = setAppState Configuration


initializeNetworkConnections : App -> App
initializeNetworkConnections = setAppState (ConnectingToNetwork (NM.NameDefinition NM.Untouched))

view : App -> Html Msg
view { storedData, appState } =
    let 
        mapAndWrap evt pgId = Html.main_
            [ Hats.classList 
                [ ("nogfx", storedData.settings.graphicsMode == GM.TextMode)
                , ("battle", False)
                , ("twodgfx", False)
                , ("threedgfx", False)
                ]
            , Maybe.withDefault "" pgId |> Hats.id
            ]
            >> Html.map evt
    in
    case (storedData.settings.graphicsMode, appState) of
        (_, MainMenu) -> 
            NoGFX.mainMenuView
            |> mapAndWrap MainMenuMsg Nothing
        (_, ConnectingToNetwork nmod) ->
            NM.view storedData nmod
            |> mapAndWrap NetworkMenuMsg Nothing
        (_, Lobby lmod) -> 
            LB.view lmod
            |> mapAndWrap LobbyMsg (Just "lobby")
        (_, PlayerSetup _ psmod) ->
            NoGFX.playerSetupView psmod
            |> mapAndWrap PlayerSetupMsg Nothing
        (GM.SvgMode x, Configuration) ->
            LowGFX.settingsView (GM.SvgMode x)
            |> Html.main_ []
        (_, Configuration) ->
            SM.view storedData.settings
            |> Html.main_ []
        (_, Play pm) ->
            NoGFX.playView pm
            |> Html.main_ [ Hats.classList [("nogfx", True), ("battle", True)] ]
            |> Html.map PlayMsg
        _ ->
            Html.main_ [] []

