port module VsWorker exposing (main)

import Logic exposing (BattleAction(..), Army, BattleState, initializeBattle)

main = Platform.worker 
        { init = init
        , update = update
        , subscriptions = subscriptions
        }

port recvLocal : (String -> msg) -> Sub msg
port recvRemote : (String -> msg) -> Sub msg

type alias Name = String
type alias Address = String

type Msg
        = UpdateBattleState BattleAction
        | SetPlayer1 Datum 
        | SetPlayer2 Datum
        | BroadcastState

type Datum
        = NameData Name
        | ArmyData Army

type Player
        = Local Name 
        | Remote Name Address 
        | AI 

type Model
        = Initialized Name
        | InitializedSVD Name Name
        | AIInitialized
        | PlayerConnected Name Name Address 
        | GameReady BattleState
        | Closed


type alias ActiveModel =
        { history : List BattleAction
        , mode : CommunicationMode
        , players : (Player, Player)
        , battle : BattleState
        }



update msg model =
        case (msg, model) of -> 
                (UpdateBattleState bAction, GameReady bs) ->
                        case bs of 
                                Battle bm ->
                                       updateBattleState bAction bm
                                _ -> bs
                (SetPlayer1 (NameData name), Initialized n) ->
                        Initialized name
                (SetPlayer1 (NameData name), InitializedSvd n1 n2) ->
                        InitializedSVD name n2
                (SetPlayer2 (NameData name), Initialized n) ->
                        PlayerConnected n name ""
                (SetPlayer2 (NameData name), InitializedSVD n1 n2) ->
                        InitializedSVD n1 name
                (SetPlayer1 (ArmyData name), InitializedSvd n1 n2) ->
                        InitializedSVD name n2
                (SetPlayer2 (ArmyData name), Initialized n) ->
                        PlayerConnected n name ""

                        
                        { model | players = Tuple.mapSecond (\_ -> name) model.players }
                SetPlayer1 (Army army) ->
                        { model2




-- game logic

