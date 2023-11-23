module ConnectionData exposing (..)

type alias ConnectionData =
    { host : String
    , port_ : Int
    , username : String
    }

init : String -> Int -> String -> ConnectionData
init = ConnectionData

connectionString : ConnectionData -> String
connectionString connectionData =
    connectionData.username ++ "@" ++ connectionData.host ++ ":" ++ (String.fromInt connectionData.port_)
