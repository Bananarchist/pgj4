module Msg exposing (Msg(..))

import MainMenu as MM
import NetworkMenu as NM
import Lobby as LB
import PlayerSetup as PS
import Play as P
import GraphicsMode as GM
import LocalStorage as LS

type Msg
    = MainMenuMsg MM.Msg
    | NetworkMenuMsg NM.Msg
    | LobbyMsg LB.Msg
    | PlayerSetupMsg PS.Msg
    | ChangeGraphicsMode GM.GraphicsMode
    | EnableAnimations
    | DisableAnimations
    | ReturnToMenu
    | PlayMsg P.Msg
    | LocalStorageRetrieved LS.LocalStorage 


