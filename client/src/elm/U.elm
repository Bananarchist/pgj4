module U exposing (..)

import Html


duple : a -> ( a, a )
duple a =
    ( a, a )


uncurry : (a -> b -> c) -> ( a, b ) -> c
uncurry fn ( a, b ) =
    fn a b


tupleList : ( a, a ) -> List a
tupleList ( a, b ) =
    [ a, b ]


mapTuple : (a -> b) -> ( a, a ) -> ( b, b )
mapTuple fn =
    Tuple.mapBoth fn fn

flipTuple : (a, b) -> (b, a)
flipTuple (a, b) = (b, a)

bool : a -> a -> Bool -> a
bool t f q =
    if q then t else f

maybeTuple : (Maybe a, Maybe b) -> Maybe (a, b)
maybeTuple t =
    case t of
        (Just a, Just b) -> Just (a, b)
        _ -> Nothing

equalsAnyOf : List a -> a -> Bool
equalsAnyOf tests value =
    case tests of
        [] -> False
        (x :: xs) -> if x == value then True else equalsAnyOf xs value


recurse : Int -> (a -> a) -> a -> a
recurse times fn val =
    if times > 0 then
        recurse (times - 1) fn (fn val)

    else
        val


singletonText =
    Html.text >> List.singleton
