module Server exposing (..)

import Json.Decode as D
import Json.Encode as E
import BattleLogic

type alias UUID = String
type alias Username = String

type alias User =
    { username : Username
    , userId : UUID
    }


type alias Game = String

type Msg
    = Message Message
    | Response Response 
    | Ignored

type Message
    = SetName String
    | HostGame String
    | ListGames
    | ListUsers
    | JoinGame String
    | SetPieces (List BattleLogic.Piece)
    | Play BattleLogic.BattleAction
    | Forfeit
    | SendChat String

type Response
    = UserJoined String
    | NameSet String
    | UserList ( List User) 
    | GameAdded String String
    | PiecesRequested Int
    | GameBegun Int BattleLogic.Army
    | NewGameState BattleLogic.TurnRecap BattleLogic.PlayerModel BattleLogic.PlayerModel
    | ActionRequested
    | Failed String
    | GameList (List Game)
    | Chat String String String

encode : E.Value -> String
encode =
    E.encode 0 

setName : String -> E.Value
setName =
    E.string >> Tuple.pair "name" >> List.singleton >> msgField "SetName" >> E.object

sendChat : String -> E.Value
sendChat =
    E.string >> Tuple.pair "content" >> List.singleton >> msgField "SendChat" >> E.object

msgField : String -> List (String, E.Value) -> List (String, E.Value)
msgField msg = (::) ("msg", E.string msg) 

decode : String -> Msg
decode =
    D.decodeString messageDecoder
    >> Result.withDefault Ignored

messageDecoder : D.Decoder Msg
messageDecoder =
    D.oneOf [ infoDecoder, eventDecoder ]

eventDecoder : D.Decoder Msg
eventDecoder =
    D.field "event" D.string
        |> D.andThen (\event ->
            case event of
                "name-set" -> nameSetDecoder
                _ -> D.fail "Unrecognized event"
            )

nameSetDecoder : D.Decoder Msg
nameSetDecoder =
    D.map (Message << SetName)
        (D.field "message" D.string)

infoDecoder : D.Decoder Msg
infoDecoder = 
    D.field "info" D.string
        |> D.andThen (\info ->
            case info of
                "game-list" -> gameListDecoder
                "user-list" -> userListDecoder
                _ -> D.fail "Unrecognized event"
            )
gameListDecoder : D.Decoder Msg
gameListDecoder =
    D.map (Response << GameList)
        (D.field "data" (D.list gameDecoder))


gameDecoder : D.Decoder Game
gameDecoder =
    D.string

userListDecoder : D.Decoder Msg
userListDecoder =
    D.map (Response << UserList)
        (D.field "data" (D.list userDecoder))

userDecoder : D.Decoder User
userDecoder =
    D.map2 User
        (D.field "username" D.string)
        (D.field "userId" D.string)

