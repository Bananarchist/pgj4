module PiecePlacement exposing (..)
import BattleLogic exposing (Piece(..), Honesty(..), Army(..), Hue(..))
import Basics.Extra exposing (flip)

type Valid = Valid

type alias PendingArmy = Result (List BattleLogic.Piece) BattleLogic.Army

type Model 
        = Incomplete 
                (List Model) 
                (Maybe Hue) 
                (Maybe Honesty) 
                PendingArmy 
        | Complete (List Model) BattleLogic.Army

type Msg
        = SetColor Hue
        | SetHonesty Honesty
        | CompletePiece
        | AddPiece Hue Honesty
        | Undo
        | Confirm

init : Model
init =
        Incomplete [] Nothing Nothing (Err [])

pieceCount : Model -> Int
pieceCount m =
        selectedPieces m |> List.length

selectedPieces : Model -> List BattleLogic.Piece
selectedPieces m =
        case m of
                Incomplete _ _ _ (Err l) -> l
                Incomplete _ _ _ (Ok a) -> BattleLogic.armyPiecesList a
                Complete _ a -> BattleLogic.armyPiecesList a

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
        let
                newLast m =
                        case m of 
                                Incomplete l c h p -> 
                                        Incomplete (model :: l) c h p
                                Complete l a -> 
                                        Complete (model :: l) a
                none = flip Tuple.pair Cmd.none
                complete m =
                        case m of
                                Incomplete l c h (Ok a) -> Complete l a
                                _ -> m
                updateHistory = newLast >> none
        in
        case (model, msg) of
                (Incomplete last _ hon pp, SetColor color) ->
                        Incomplete last (Just color) hon pp 
                        |> updateHistory
                (Incomplete last color _ pp, SetHonesty hon) ->
                        Incomplete last color (Just hon) pp
                        |> updateHistory
                (Incomplete l (Just c) (Just h) pp, CompletePiece) ->
                        pp
                        |> withPlayerPiece (Piece c h)
                        |> Incomplete l (Just c) (Just h)
                        |> newLast
                        |> complete
                        |> none
                (Incomplete last c h pp, AddPiece nc nh) ->
                        pp
                        |> withPlayerPiece (Piece nc nh)
                        |> Incomplete last c h 
                        |> newLast
                        |> complete
                        |> none
                (Incomplete (last :: _) c h pp, Undo) ->
                        none last
                (Complete (last :: _) _, Undo) ->
                        none last
                _ ->
                        none model


withPlayerPiece : Piece -> PendingArmy -> PendingArmy
withPlayerPiece p pp =
        case pp of 
                Err pieces -> 
                        pieces ++ [ p ]
                        |> (if List.length pieces == 4 then BattleLogic.armyFromList >> Ok else Err)
                _ -> pp

validか : Model -> Bool
validか pp =
        case pp of 
                Complete _ _ -> True
                _ -> False
