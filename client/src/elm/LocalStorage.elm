module LocalStorage exposing (..)

import Settings as S
import ConnectionData as CD
import GraphicsMode as GM

init : LocalStorage
init = 
    { settings = S.init
    , sameScreenNames = []
    , connectionData = []
    }

type alias LocalStorage = 
    { settings : S.Settings
    , sameScreenNames : List String
    , connectionData : List CD.ConnectionData
    }

mapSettings : (S.Settings -> S.Settings) -> LocalStorage -> LocalStorage
mapSettings fn ({settings} as ls) =
    { ls | settings = fn settings }

addConnectionDataEntry : CD.ConnectionData -> LocalStorage -> LocalStorage
addConnectionDataEntry entry ({connectionData} as ls) =
    { ls | connectionData = 
        if List.any (\cd -> cd.username == entry.username && cd.port_ == cd.port_ && cd.host == cd.host) connectionData then 
            connectionData 
        else 
            entry :: connectionData }

keys : List String
keys =
    [ "settings"
    , "sameScreenNames"
    , "connectionData"
    ]
