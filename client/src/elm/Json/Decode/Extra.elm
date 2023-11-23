module Json.Decode.Extra exposing (..)

import Json.Decode  exposing (..)
import ConnectionData exposing (ConnectionData)
import GraphicsMode exposing (GraphicsMode(..))
import Settings exposing (Settings)
import LocalStorage exposing (LocalStorage)

connectiondata : Decoder ConnectionData
connectiondata =
    map3 ConnectionData
        (field "host" string)
        (field "port" int)
        (field "username" string)

graphicsMode : Decoder GraphicsMode
graphicsMode =
    string
    |> andThen
        (\s -> case s of
            "TextMode" -> succeed TextMode
            "GLMode True" -> succeed <| GLMode True
            "GLMode False" -> succeed <| GLMode False
            "SvgMode True" -> succeed <| SvgMode True
            "SvgMode False" -> succeed <| SvgMode False
            _ -> fail ("Unknown graphicscoder " ++ s)
        )

settings : Decoder Settings
settings =
    map2 Settings
        (field "graphicsMode" graphicsMode)
        (field "saveSensitiveData" bool)

localStorage : Decoder LocalStorage
localStorage =
    map3 LocalStorage
        (field "settings" settings)
        (field "sameScreenNames" (list string))
        (field "connectionData" (list connectiondata))
