port module Ports exposing (..)

import Dict exposing (Dict)
import Json.Encode as E
import Json.Encode.Extra as E
import Json.Decode as D
import Json.Decode.Extra as D
import ConnectionData exposing (ConnectionData)
import GraphicsMode exposing (GraphicsMode, enableAnimations, disableAnimations)
import LocalStorage as LS
import U


port initializeWebSocket : String -> Cmd msg
port send : String -> Cmd msg
port socketMessageReceiver : (String -> msg) -> Sub msg
port socketOpened : (String -> msg) -> Sub msg
port socketClosed : (String -> msg) -> Sub msg
port saveLocalStorage : String -> Cmd msg
port setLocalStorage : String -> Cmd msg
port clearLocalStorage : () -> Cmd msg
port getLocalStorage : List String -> Cmd msg
port localStorage : (String -> msg) -> Sub msg


openWebSocket : ConnectionData -> Cmd msg
openWebSocket =
    E.connectionData
    >> E.encode 0
    >> initializeWebSocket

getLocalStorageKeys : Cmd msg
getLocalStorageKeys =
    getLocalStorage LS.keys

localStorageListener : (LS.LocalStorage -> msg) -> Sub msg
localStorageListener fn =
    localStorage (D.decodeString D.localStorage >> Result.withDefault LS.init >> fn)

saveLocalStorageCmd : LS.LocalStorage -> Cmd msg
saveLocalStorageCmd =
    E.localStorage
    >> E.encode 0
    >> saveLocalStorage

