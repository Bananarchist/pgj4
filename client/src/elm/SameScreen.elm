module SameScreen exposing (..)

import Play

type State
    = P1Recap
    | P1Act
    | P1Review
    | P1Surrender
    | P2Recap
    | P2Act
    | P2Review
    | P2Surrender

type Model 
    = Model State Play.Model

type Msg 
    = PlayMsg Play.Msg
    | Advance
    | Return

update : Msg -> Model -> (Model, Cmd Msg)
update msg (Model state pmod) =
    case (state, Play.state pmod, msg) of
        (P1Act, _, PlayMsg pmsg) -> 
            let (newPMod, cmds) = Play.update pmsg pmod
            in
            if 
        (P2Act, _, PlayMsg pmsg) -> Play.update pmsg pmod |> Tuple.mapFirst (Model state)
        (P1Recap, _, Advance) -> (Model P1Act pmod, Cmd.none)
        (P1Review, _, Advance) -> (Model P1Surrender pmod, Cmd.none)
        (P1Surrender, _, Advance) -> (Model P2Recap pmod, Cmd.none)
        (P2Recap, _, Advance) -> (Model P2Act pmod, Cmd.none)
        (P2Review, _, Advance) -> (Model P2Surrender pmod, Cmd.none)
        (P2Surrender, _, Advance) -> (Model P1Recap pmod, Cmd.none)
        _ -> (model, Cmd.none)

