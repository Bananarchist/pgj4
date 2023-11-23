module Menu exposing (Menu, view, init, update, Msg)
import Html exposing (Html)

type Menu
        = YesNo
        | Multi 
        | Cons (Menu, Menu)

type Msg
        = Msg

type Model msg = 
        Model
                { menu : Menu
                , options : Menu -> List msg
                , labels : Menu -> List String
                , view : String -> msg -> Html msg
                , update : Menu -> (Menu, List (Cmd msg))
                , subscriptions : Menu -> List (Sub msg)
                }

view : Model msg -> List (Html msg)
view (Model m) =
        List.map2 m.view (m.labels m.menu) (m.options m.menu)


type MainMenuChoice 
        = Settings

type SettingsField
        = GraphicsMode

type UI
        = MainMenu 
                { selected : Maybe MainMenuChoice }
        | PieceSelection
                { selected : Maybe Int }
        | Battle
                { selected : Maybe Int }
        | Settings
                { selected : Maybe SettingsField
                }
        | TextBox 
