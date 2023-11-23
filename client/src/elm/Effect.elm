module Effect exposing (..)

import Shared
import Task
import Msg exposing (Msg)

type Effect msg
    = None
    | Cmd (Cmd msg)
    --| Shared Shared.Msg
    | Batch (List (Effect msg))
    
effectToCmd : Effect msg -> Cmd msg
effectToCmd e = 
    case e of
        None -> Cmd.none
        Cmd m -> m
        --Shared m -> Task.succeed m |> Task.perform identity
        Batch effects -> Cmd.batch (List.map effectToCmd effects)

{-
MainMenu -> Settings settings
MainMenu -> Credits
MainMenu -> PieceSettings settings AIGame
MainMenu -> PieceSettings settings SameScreen
MainMenu -> NetworkSetup settings

NetworkSetup -> MainMenu
NetworkSetup -> Lobby settings

Lobby -> PieceSettings settings NetworkGame

PieceSettings -> GamePlay settings gameMode
PieceSettings -> SwapScreen settings


GoToMainMenu
GoToSettingsMenu
GoToCredits
GoToNetworkMenu
GoTo


a -> Msg
a = (b -> c -> d -> e)

-}

