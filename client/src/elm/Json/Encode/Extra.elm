module Json.Encode.Extra exposing (..)

import Json.Encode exposing (..)
import ConnectionData exposing (ConnectionData)
import GraphicsMode exposing (GraphicsMode(..))
import Settings exposing (Settings)
import LocalStorage exposing (LocalStorage)

connectionData : ConnectionData -> Value
connectionData cdata = 
    object
        [ ("host", string cdata.host)
        , ("port", int cdata.port_)
        , ("username", string cdata.username)
        ]

graphicsMode : GraphicsMode -> Value
graphicsMode gm =
    case gm of
        GLMode True -> string "GLMode True"
        GLMode False -> string "GLMode False"
        SvgMode True -> string "SvgMode True"
        SvgMode False -> string "SvgMode False"
        TextMode -> string "TextMode"


settings : Settings -> Value
settings s =
    object
        [ ("graphicsMode", graphicsMode s.graphicsMode)
        , ("saveSensitiveData", bool s.saveSensitiveData)
        ]

localStorage : LocalStorage -> Value
localStorage ls =
    object
        [ ("settings", settings ls.settings)
        , ("sameScreenNames", list string ls.sameScreenNames)
        , ("connectionData", list connectionData ls.connectionData)
        ]
