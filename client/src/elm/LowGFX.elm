module LowGFX exposing (..)

import Html exposing (Html)
import Html.Attributes as Hats
import Html.Events as Emit
import Settings as S
import GraphicsMode as GM
import Msg
import U

settingsView : GM.GraphicsMode -> List (Html Msg.Msg)
settingsView gm =
    settingsGraphicsDropDown

settingsGraphicsDropDown : List (Html Msg.Msg)
settingsGraphicsDropDown =
    [ Html.fieldset []
        [ Html.label [ Hats.for "graphics-dd" ] [ Html.text "Graphics Mode" ]
        ]
    ]

settingsReturnButton : List (Html Msg.Msg)
settingsReturnButton =
    [ Html.button [ Emit.onClick Msg.ReturnToMenu ] [ Html.text "Back" ] ]

