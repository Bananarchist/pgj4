module GraphicsMode exposing (..)

import Json.Decode as D
import Json.Encode as E

{-
type alias ToodyGraphics =
    { lastFrame : Int
    , animations : Bool
    }

type alias ThreedyGraphics =
    {  lastFrame : Int 
    ...
-}

type GraphicsMode
    = TextMode
    | GLMode Bool
    | SvgMode Bool

animationEnabledか : GraphicsMode -> Bool
animationEnabledか gm =
    case gm of
        GLMode x -> x
        SvgMode x -> x
        TextMode -> False


enableAnimations : GraphicsMode -> GraphicsMode
enableAnimations gm =
    case gm of
        GLMode _ -> GLMode True
        SvgMode _ -> SvgMode True
        _ -> gm

disableAnimations : GraphicsMode -> GraphicsMode
disableAnimations gm =
    case gm of
        GLMode _ -> GLMode False
        SvgMode _ -> SvgMode False
        _ -> gm

isGLMode gm = 
    case gm of
        GLMode _ -> True
        _ -> False

isSvgMode gm = 
    case gm of
        SvgMode _ -> True
        _ -> False



isTextMode gm = 
    case gm of
        TextMode -> True
        _ -> False


