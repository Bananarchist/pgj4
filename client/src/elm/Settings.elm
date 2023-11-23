module Settings exposing (..)

import GraphicsMode exposing (GraphicsMode(..))
import Html exposing (Html)

type alias Settings =
    { graphicsMode : GraphicsMode
    , saveSensitiveData : Bool
    }

init : Settings
init = 
    { graphicsMode = TextMode
    , saveSensitiveData = True
    }


