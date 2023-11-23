module Fuzz.Extra exposing (..)

import Fuzz exposing (Fuzzer)
import BattleLogic as BL
import ConnectionData as CD

hue : Fuzzer BL.Hue
hue =
    Fuzz.oneOfValues
        [ BL.Orange
        , BL.Blue
        ]

honesty : Fuzzer BL.Honesty
honesty =
    Fuzz.oneOfValues
        [ BL.Authentic
        , BL.Deceitful
        ]

piece : Fuzzer BL.Piece
piece =
    Fuzz.map2 BL.Piece hue honesty

army : Fuzzer BL.Army
army =
    Fuzz.map5 BL.Army piece piece piece piece piece

playerModel : Fuzzer BL.PlayerModel
playerModel =
    Fuzz.map BL.initialPlayerModel army

battleModel : Fuzzer BL.BattleModel
battleModel =
    Fuzz.map2 BL.init playerModel playerModel

connectionData : Fuzzer CD.ConnectionData
connectionData =
    Fuzz.map3 CD.ConnectionData Fuzz.string Fuzz.int Fuzz.string
