module SettingsMenu exposing (..)

import Settings exposing (..)
import GraphicsMode exposing (..)
import Html exposing (Html)
import Msg
import Html.Attributes as Hats
import Json.Decode as D
import Json.Decode.Extra as D
import Json.Encode as E
import Json.Encode.Extra as E
import Html.Events as Emit
import U

view : Settings -> List (Html Msg.Msg)
view settings =
    settingsGraphicsDropDown settings
    ++ settingsAnimationToggle settings
    ++ settingsReturnButton



enableSvgMode = SvgMode True |> E.graphicsMode |> E.encode 0
enableGLMode = GLMode True |> E.graphicsMode |> E.encode 0
enableTextMode = TextMode |> E.graphicsMode |> E.encode 0
decodeEmission = D.decodeString D.graphicsMode >> Result.withDefault TextMode >> Msg.ChangeGraphicsMode

settingsGraphicsDropDown : Settings -> List (Html Msg.Msg)
settingsGraphicsDropDown {graphicsMode} =
    [ Html.fieldset []
        [ Html.label [ Hats.for "graphics-dd" ] [ Html.text "Graphics Mode" ]
        , Html.select [ Emit.onInput decodeEmission ]
            [ Html.option 
                [ Hats.value enableSvgMode
                , Hats.selected <| isSvgMode graphicsMode ] 
                [ Html.text "2D GFX" ]
            , Html.option 
                [ Hats.value enableGLMode
                , Hats.selected <| isGLMode graphicsMode ] 
                [ Html.text "3D GFX" ]
            , Html.option 
                [ Hats.value enableTextMode
                , Hats.selected <| isTextMode graphicsMode ] 
                [ Html.text "Text-based" ]
            ]
        ]
    ]

settingsReturnButton : List (Html Msg.Msg)
settingsReturnButton =
    [ Html.button [ Emit.onClick Msg.ReturnToMenu ] [ Html.text "Back" ] ]

settingsAnimationToggle : Settings -> List (Html Msg.Msg)
settingsAnimationToggle {graphicsMode} =
    [ Html.fieldset []
        [ Html.label [ Hats.for "animation-checkbox" ] [ Html.text "Enable animations" ]
        , Html.input [ Hats.type_ "checkbox", Hats.checked (animationEnabledか graphicsMode), Emit.onCheck (U.bool Msg.EnableAnimations Msg.DisableAnimations)] []
        ]
    ]

