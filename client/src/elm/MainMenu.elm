module MainMenu exposing (init, subscriptions, Model, Msg(..), menuText)

-- import Shared exposing (Shared)
--import Effect exposing (Effect)
import Html
import Html.Events as Emit

type Model = Model

type Msg
    = StartLocalGame
    | StartAIGame
    | ConnectToNetwork
    | AdjustSettings
    | ViewCredits


menuText : Msg -> String
menuText msg =
    case msg of
        StartLocalGame -> "Play Local"
        StartAIGame -> "Play the AI"
        ConnectToNetwork -> "Network Play"
        AdjustSettings -> "Settings"
        ViewCredits -> "Credits"

init = Model

subscriptions _ = Sub.none
