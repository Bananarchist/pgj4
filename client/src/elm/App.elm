module App exposing (App(..))


type alias App =
    { update : Input.Msg -> App -> (App, Cmd Msg)
    , draw : Duration -> App -> App
    , network : Duration -> App -> (App, Cmd Msg)
    }


