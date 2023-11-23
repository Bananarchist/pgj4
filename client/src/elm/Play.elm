module Play exposing (..)

import BattleLogic as BL
import U
import Basics.Extra exposing (flip)
import BattleLogic exposing (PlayerModel)
import ConnectionData exposing (ConnectionData)
import Html exposing (Html)

type SameScreenPhase
    = Initial 
    | ActPhase
    | ReviewPhase 
    | TransitionPhase 
    | Recap 
    | Player2 SameScreenPhase
    {-
    | P1Phase
    | P1PhaseReview
    | P1TransitionToP2
    | P2Phase
    | P2PhaseReview
    | P2TransitionToP1
    | Recap
    -}

type OpponentType
    = Net ConnectionData String
    | Bot
    | Man SameScreenPhase

{-
type alias Model =
    { primaryPlayerModel : PlayerModel
    , secondaryPlayerModel : Maybe PlayerModel
    , connectionData : Maybe ConnectionData
    , opponentType : OpponentType
    , battleState : Maybe BattleLogic.BattleState
    , whoView: Player
    } 
-}


type Preparation
    = Singleplayer (Maybe ClientState) PlayerModel
    | Multiplayer SameScreenPhase PlayerModel PlayerModel

{-
type Competition
    = Network ClientState BL.BattleState
    | AI BL.BattleState
    | SameScreen SameScreenPhase (String, String) BL.BattleState
-}

type ClientState
    = ClientTurn String ConnectionData
    | ClientAwaiting String ConnectionData

type Player
    = One
    | Two

type OpponentState
    = SharedScreen Player
    | AI
    | Network ConnectionData 

type alias TargetPlayer = PlayerModel
type alias AwaitingPlayer = PlayerModel

{-
type State
    = CompletelySetup OpponentState (String, String) BL.BattleState
    | PlayerSettingUp 
    | PrimaryPlayerSettingUp OpponentState PlayerModel (Maybe PlayerModel)
    | SecondaryPlayerSettingUp OpponentState PlayerModel PlayerModel
    | Competing OpponentState (String, String) BL.BattleState
    | CompletedGame OpponentState (String, String) BL.BattleState
    | TransitionScreen
-}


type alias PlayerData =
    { player1 : String
    , player2 : String
    }

nameOfPlayer : Player -> (PlayerData -> String)
nameOfPlayer player = 
    case player of
        One -> .player1
        Two -> .player2

rotate : Player -> Player
rotate p =
    case p of
        One -> Two
        Two -> One

type alias ViewData = Player
    --{ turnToView : Player 
    --}

type Msg
    = BattleMsg BL.BattleAction
    | Confirm

type Model
    = ReturningToMenu
    | CompletedGame Player PlayerData BL.BattleState
    | Transitioning Player PlayerData BL.BattleState
    | Competing Player PlayerData BL.BattleState


view : Model -> Html Msg
view model =
    Html.text "not implemented"
    {-
    case model of
        CompletedGame player playerData _ -> {- based on state ... -} Html.text (nameOfPlayer player playerData |> flip (++) " wins!")
        Competing One {player1, player2} bs -> Html.text ("YOUR TURN ASS-PLAYER" ++ player1)
        Competing Two {player1, player2} bs -> Html.text ("YOUR TURN ASS-PLAYER" ++ player2)
        Transitioning player playerData _ -> Html.text (nameOfPlayer player playerData |> flip (++) " wins!")
        -}


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    (model, Cmd.none)
    {-
    case (model, msg) of
        (CompletedGame _ _ _, Confirm) -> ReturningToMenu
        (Transitioning One playerData bs, Confirm) -> Competing Two playerData bs
        (Transitioning Two playerData bs, Confirm) -> Competing One playerData bs
        (Competing One playerData ((BS.Battle _ (BS.TurnEnded :: _, _)) as bs), Confirm) -> Transitioning Two playerData bs
        (Competing Two playerData ((BS.Battle _ (BS.TurnEnded :: _, _)) as bs), Confirm) -> Transitioning One playerData bs
        (Competing player playerData bs, BattleMsg bsmsg) -> Competing player playerData (BS.update bs bsmsg)
    -}


initSameScreen : (String, BL.Army) -> (String, BL.Army) -> Model
initSameScreen p1 p2 =
    ReturningToMenu
    {-
    ( BL.initialPlayerModel (Tuple.second p1)
    , BL.initialPlayerModel (Tuple.second p2)
    )
    |> U.uncurry BL.init
    |> U.duple
    |> Tuple.mapSecond (Tuple.pair [])
    |> U.uncurry BL.Battle
    |> SameScreen Initial (Tuple.first p1, Tuple.first p2)
    -}

none = flip Tuple.pair Cmd.none

{-
update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case (model, msg) of
        (SameScreen Initial names bs, SameScreenTransition) ->
            SameScreen PlayPhase names bs |> none
        (SameScreen PlayPhase names bs, BLMsg blm) -> 
            let 
                nbs = BL.updateBattleState blm bs
                nssp = 
                    if 
                        BL.player1TurnStart nbs
                        || BL.player2TurnStart nbs
                    then
                        ReviewPhase
                    else
                        PlayPhase
            in
            SameScreen nssp names nbs
            |> none
        (SameScreen ReviewPhase names bs, SameScreenTransition) ->
            SameScreen TransitionPhase names bs |> none
        (SameScreen TransitionPhase names bs, SameScreenTransition) ->
            SameScreen PlayPhase names bs |> none
        {-
        (SameScreen P1Phase names bs, SameScreenTransition) ->
            SameScreen P1PhaseReview names bs |> none
        (SameScreen P1PhaseReview names bs, SameScreenTransition) ->
            SameScreen P1TransitionToP2 names bs |> none
        (SameScreen P1TransitionToP2 names bs, SameScreenTransition) ->
            SameScreen P2Phase names bs |> none
        (SameScreen P2Phase names bs, SameScreenTransition) ->
            SameScreen P2PhaseReview names bs |> none
        (SameScreen P2PhaseReview names bs, SameScreenTransition) ->
            SameScreen P2TransitionToP1 names bs |> none
        (SameScreen P2TransitionToP1 names bs, SameScreenTransition) ->
            SameScreen P1Phase names bs |> none
        -}
        _ ->
            (model, Cmd.none)

        -}
