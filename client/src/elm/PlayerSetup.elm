module PlayerSetup exposing (..)

import BattleLogic exposing (Piece(..), Honesty(..), Army(..), Hue(..))
import Basics.Extra exposing (flip)
import PiecePlacement as PP
import U

type PlayerModel 
    = Unnamed
    | Naming String (Maybe PP.Model)
    | Named String PP.Model

type Model
    = AI PlayerModel
    | Network PlayerModel (Maybe (String, BattleLogic.Army))
    | WaitingOnNetwork (String, BattleLogic.Army)
    | SameScreen1 PlayerModel (Maybe PlayerModel)
    | SameScreenTransitionTo2 PlayerModel (Maybe PlayerModel)
    | SameScreen2 PlayerModel PlayerModel
    | SameScreenTransitionTo1 PlayerModel PlayerModel
    | ReturningToMenu
    | StartingNetworkGame (String, BattleLogic.Army) (String, BattleLogic.Army)
    | StartingAIGame (String, BattleLogic.Army) 
    | StartingSameScreenGame (String, BattleLogic.Army) (String, BattleLogic.Army)

type Msg
    = PlayerModelMsg PlayerModelMsg
    | StartGame
    | OpponentDisconnected
    | OpponentArmyReady String BattleLogic.Army
    | ReturnToMenu

type PlayerModelMsg
    = SetName String
    | PiecePlacementMsg PP.Msg
    | Confirm
    | GoBack

setName : String -> Msg
setName = PlayerModelMsg << SetName

setPiece : PP.Msg -> Msg
setPiece = PlayerModelMsg << PiecePlacementMsg

init : Model
init = initAI

initSameScreen : Model
initSameScreen = SameScreen1 Unnamed Nothing

initNetwork : String -> Model
initNetwork s =
    Network (Named s PP.init) Nothing

initAI : Model
initAI = AI (Named "Human" PP.init)

transitioningか : Model -> Bool
transitioningか m = 
    case m of
        SameScreenTransitionTo1 _ _ -> True
        SameScreenTransitionTo2 _ _ -> True
        _ -> False

activePlayerModel : Model -> Maybe PlayerModel
activePlayerModel m =
    case m of 
        AI pm -> Just pm
        Network pm _ -> Just pm
        WaitingOnNetwork _ -> Nothing
        SameScreen1 pm _ -> Just pm
        SameScreenTransitionTo2 _ mbum2 -> mbum2
        SameScreen2 _ pm -> Just pm
        SameScreenTransitionTo1 pm _ -> Just pm
        ReturningToMenu -> Nothing
        _ -> Nothing


playerModelCompleteか : PlayerModel -> Bool
playerModelCompleteか um =
    case um of
        Named _ (PP.Complete _ _) -> True
        Naming s (Just (PP.Complete _ _)) -> not <| String.isEmpty s 
        _ -> False

both fn cmb um1 = Tuple.pair um1 >> U.mapTuple fn >> U.uncurry cmb

completeか : Model -> Bool
completeか m =
    case m of
        AI um -> playerModelCompleteか um
        Network um _ -> playerModelCompleteか um
        WaitingOnNetwork _ -> True
        SameScreen1 um1 (Just um2) ->
            both playerModelCompleteか (&&) um1 um2
        SameScreen2 um1 um2 ->
            both playerModelCompleteか (&&) um1 um2
        StartingAIGame _ -> True
        StartingSameScreenGame _ _ -> True
        _ -> False

armies : Model -> List (BattleLogic.Army)
armies model =
    case model of
        Network (Named _ (PP.Complete _ a)) _ -> [ a ]
        AI (Named _ (PP.Complete _ a)) -> [ a ]
        SameScreen1 (Named _ (PP.Complete _ a1)) (Just (Named _ (PP.Complete _ a2))) -> [ a1, a2 ]
        SameScreen2 (Named _ (PP.Complete _ a1)) (Named _ (PP.Complete _ a2)) -> [ a1, a2 ]
        SameScreenTransitionTo1 (Named _ (PP.Complete _ a1)) (Named _ (PP.Complete _ a2)) -> [ a1, a2 ]
        SameScreenTransitionTo2 (Named _ (PP.Complete _ a1)) (Just (Named _ (PP.Complete _ a2))) -> [ a1, a2 ]
        _ -> []



none = flip Tuple.pair Cmd.none


updatePlayerModel : PlayerModelMsg -> PlayerModel -> (PlayerModel, Cmd PlayerModelMsg)
updatePlayerModel msg um =
    case (um, msg) of
        (Unnamed, SetName s) -> (Naming s Nothing, Cmd.none)
        (Naming _ mbPP, SetName newS) -> (Naming newS mbPP, Cmd.none)
        (Naming s (Just pp), Confirm) -> (Named s pp, Cmd.none)
        (Naming s Nothing, Confirm) -> (Named s PP.init, Cmd.none)
        (Named s pp, GoBack) -> (Naming s (Just pp), Cmd.none)
        (Named s pp, PiecePlacementMsg ppmsg) ->
            PP.update ppmsg pp
            |> Tuple.mapBoth 
                (Named s) 
                (Cmd.map PiecePlacementMsg)
        _ -> um |> none

exitableStateか : Model -> Bool
exitableStateか m =
    case m of
        SameScreen1 Unnamed _ -> True
        SameScreen1 (Naming _ _) _ -> True
        AI Unnamed -> True
        AI (Naming _ _) -> True
        Network Unnamed _ -> True
        Network (Naming _ _) _ -> True
        _ -> False

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case (model, msg) of
        ( SameScreen1 (Named s pp) mbum2, PlayerModelMsg Confirm ) ->
            SameScreenTransitionTo2 (Named s pp) mbum2
            |> none
        ( SameScreen2 (Named p1n (PP.Complete _ p1army)) (Named p2n (PP.Complete _ p2army)), StartGame) ->
            StartingSameScreenGame (p1n, p1army) (p2n, p2army) |> none
        ( SameScreen1 um1 mbum2, PlayerModelMsg umm ) ->
            updatePlayerModel umm um1
            |> Tuple.mapBoth 
                (flip SameScreen1 mbum2)
                (Cmd.map PlayerModelMsg)
        ( SameScreen2 um1 um2, PlayerModelMsg umm) ->
            updatePlayerModel umm um2
            |> Tuple.mapBoth 
                (SameScreen2 um1)
                (Cmd.map PlayerModelMsg)
        ( Network um mbpm2, PlayerModelMsg umm) -> 
            updatePlayerModel umm um 
            |> Tuple.mapBoth 
                (flip Network mbpm2)
                (Cmd.map PlayerModelMsg)
        ( AI um, PlayerModelMsg umm) -> 
            updatePlayerModel umm um 
            |> Tuple.mapBoth 
                AI
                (Cmd.map PlayerModelMsg)
        ( SameScreenTransitionTo2 um1 (Just um2), PlayerModelMsg Confirm ) ->
            SameScreen2 um1 um2 |> none
        ( SameScreenTransitionTo2 um1 Nothing, PlayerModelMsg Confirm ) ->
            SameScreen2 um1 Unnamed |> none
        ( SameScreenTransitionTo1 um1 um2, PlayerModelMsg Confirm ) ->
            SameScreen1 um1 (Just um2) |> none
        ( _, OpponentDisconnected) -> 
            ReturningToMenu |> none
        ( WaitingOnNetwork p1, OpponentArmyReady p2name p2army ) ->
            StartingNetworkGame p1 (p2name, p2army) |> none
        ( Network pm _, OpponentArmyReady p2name p2army) ->
            Network pm (Just (p2name, p2army)) |> none
        ( Network (Named p1name (PP.Complete _ p1army)) (Just p2), StartGame ) ->
            StartingNetworkGame (p1name, p1army) p2 |> none
        _ -> (model, Cmd.none)




