module Logic exposing (..)

import Basics.Extra exposing (flip)

type alias Name = String
type alias Spot = Int
type alias Address = String

type Hue
        = Orange
        | Blue

type Honesty
        = Authentic
        | Deceitful

type Piece 
        = Obliterated
        | Piece Hue Honesty

honestBlue = Piece Blue Authentic
dishonestBlue = Piece Blue Deceitful
honestOrange = Piece Orange Authentic
dishonestOrange = Piece Orange Deceitful

type Army 
        = Army Piece Piece Piece Piece Piece


initialArmy = 
        Army Obliterated Obliterated Obliterated Obliterated Obliterated

type Msg 
        = SetArmy Army
        | SetName Name
        | SwitchPieces Spot Spot
        | Connected Name Address
        | FirePiece Spot
        | ForfeitGame

type alias PlayerModel = 
        { army : Army
        , health : Int
        }


type Mood
        = Intrigued
        | Tense
        | Danger
        | Excited
        | Crushed
        | Triumphant


type alias BattleModel =
        { p1 : PlayerModel
        , p2 : PlayerModel
        , phase : BattlePhase
        } 

type BattleState
        = Initialized (Maybe Army) (Maybe Army)
        | Battle BattleModel
        | Player1Victory
        | Player2Victory

type BattleAction
        = Switch Spot Spot
        | Fire Spot
        | Forfeit

type Turn
        = First
        | Second

type BattlePhase
        = Player1 Turn
        | Player2 Turn


initializeBattle =
        Initialized Nothing Nothing

startBattle army1 army2 =
    Battle { p1 = { army = army1, health = 5 }, p2 = { army = army2, health = 5 }, phase = Player1 First }

player1Health bState =
    case bState of 
        Battle { p1 } ->
            p1.health
        _ ->
            0
player2Health bState =
    case bState of 
        Battle { p2 } ->
            p2.health
        _ ->
            0

player1Army bState =
    case bState of 
        Battle { p1 } ->
            p1.army
        _ ->
            initialArmy
player2Army bState =
    case bState of 
        Battle { p2 } ->
            p2.army
        _ ->
            initialArmy
decode = 
        Nothing

encode =
        Nothing

currentTurn : BattleState -> BattlePhase
currentTurn bState =
    case bState of
        (Battle { phase }) ->
            phase
        _ ->
            Player1 First

player1TurnStart : BattleState -> Bool
player1TurnStart bState =
    case bState of
        (Battle { phase }) ->
            case phase of
                Player1 First -> True
                _ -> False
        _ -> False


player2TurnStart : BattleState -> Bool
player2TurnStart bState =
    case bState of
        (Battle { phase }) ->
            case phase of
                Player2 First -> True
                _ -> False
        _ -> False
    

updatePlayer1Army army bState = 
    case bState of 
        Battle ({ p1 } as bMod) ->
            Battle { bMod | p1 = { p1 | army = army } }
        _ ->
            bState

updatePlayer2Army army bState = 
    case bState of 
        Battle ({ p2 } as bMod) ->
            Battle { bMod | p2 = { p2 | army = army } }
        _ ->
            bState

mapPlayer1Army fn bState =
    case bState of
        Battle ({ p1 } as bMod) ->
            Battle { bMod | p1 = { p1 | army = fn bMod } }
        _ ->
            bState

mapPlayer2Army fn bState =
    case bState of
        Battle ({ p2 } as bMod) ->
            Battle { bMod | p2 = { p2 | army = fn bMod } }
        _ ->
            bState

getPieceAt spot (Army p1 p2 p3 p4 p5) =
        case spot of
                1 -> p1
                2 -> p2
                3 -> p3
                4 -> p4
                5 -> p5
                _ -> Obliterated

updatePieceAt : Spot -> Piece -> Army -> Army
updatePieceAt spot newPiece (Army p1 p2 p3 p4 p5) =
        case spot of
                1 -> Army newPiece p2 p3 p4 p5
                2 -> Army p1 newPiece p3 p4 p5
                3 -> Army p1 p2 newPiece p4 p5
                4 -> Army p1 p2 p3 newPiece p5
                5 -> Army p1 p2 p3 p4 newPiece
                _ -> Army p1 p2 p3 p4 p5

swapPiece oldSpot newSpot army =
        let
            activePiece = getPieceAt oldSpot army
            displacedPiece = getPieceAt newSpot army
        in
        army
        |> updatePieceAt newSpot activePiece
        |> updatePieceAt oldSpot displacedPiece


mapPieces fn (Army p1 p2 p3 p4 p5) =
        Army (fn p1) (fn p2) (fn p3) (fn p4) (fn p5)

mapIndexedPieces fn (Army p1 p2 p3 p4 p5) =
        Army (fn 1 p1) (fn 2 p2) (fn 3 p3) (fn 4 p4) (fn 5 p5)

armyPiecesList (Army p1 p2 p3 p4 p5) =
        [p1, p2, p3, p4, p5]


armyFromList l =
        let
                cl = (List.length l) |> (-) 5 |> flip List.repeat Obliterated |> (++) l
                p1 = List.head cl |> Maybe.withDefault Obliterated
                p2 = List.drop 1 cl |> List.head |> Maybe.withDefault Obliterated
                p3 = List.drop 2 cl |> List.head |> Maybe.withDefault Obliterated
                p4 = List.drop 3 cl |> List.head |> Maybe.withDefault Obliterated
                p5 = List.drop 4 cl |> List.head |> Maybe.withDefault Obliterated
        in
        Army p1 p2 p3 p4 p5

updateHueAt spot newHue (Army p1 p2 p3 p4 p5 as army) =
        let 
            piece = getPieceAt spot army
                |> (\p -> case p of
                        Piece _ h -> Piece newHue h
                        _ -> Piece newHue Authentic
                )
        in
        case spot of
                1 -> Army piece p2 p3 p4 p5
                2 -> Army p1 piece p3 p4 p5
                3 -> Army p1 p2 piece p4 p5
                4 -> Army p1 p2 p3 piece p5
                5 -> Army p1 p2 p3 p4 piece
                _ -> Army p1 p2 p3 p4 p5


updateHonestyAt spot newHonesty (Army p1 p2 p3 p4 p5 as army) =
        let 
            piece = getPieceAt spot army
                |> (\p -> case p of
                        Piece c _ -> Piece c newHonesty
                        _ -> Piece Orange newHonesty
                )
        in
        case spot of
                1 -> Army piece p2 p3 p4 p5
                2 -> Army p1 piece p3 p4 p5
                3 -> Army p1 p2 piece p4 p5
                4 -> Army p1 p2 p3 piece p5
                5 -> Army p1 p2 p3 p4 piece
                _ -> Army p1 p2 p3 p4 p5

killPieceAt spot =
        updatePieceAt spot Obliterated 

switchActivePieces : Spot -> Spot -> BattleModel -> BattleModel
switchActivePieces spot1 spot2 ({p1, p2, phase} as bMod) =
        case phase of 
                Player1 _ -> 
                        { bMod | p1 = { p1 | army = updatePieceAt spot2 (getPieceAt spot1 p1.army) (updatePieceAt spot1 (getPieceAt spot2 p1.army) p1.army) } }
                Player2 _ ->
                        { bMod | p2 = { p1 | army = updatePieceAt spot2 (getPieceAt spot1 p2.army) (updatePieceAt spot1 (getPieceAt spot2 p2.army) p2.army) } }


modelWithPhase p bMod =
        { bMod | phase = p }

reduceTurns bMod =
        bMod |>
                case bMod.phase of
                        Player1 First -> 
                                modelWithPhase <| Player1 Second
                        
                        Player1 Second -> 
                                modelWithPhase <| Player2 First

                        Player2 First -> 
                                modelWithPhase <| Player2 Second

                        Player2 Second -> 
                                modelWithPhase <| Player1 First


p1Model =
        .p1 
p2Model =
        .p2

victory bMod =
        case bMod.phase of
                Player1 _ ->
                        Player1Victory
                Player2 _ ->
                        Player2Victory

test bMod =
        Just bMod

hasUnits =
        let
            checkArmy bMod p =
                case p bMod of
                        Army Obliterated Obliterated Obliterated Obliterated Obliterated ->
                                Nothing
                        _ ->
                                Just bMod
        in
        Maybe.andThen (\bMod ->
                case (bMod.phase, bMod.p1.army, bMod.p2.army) of
                        (Player1 _, Army Obliterated Obliterated Obliterated Obliterated Obliterated, _) ->
                                Nothing
                        (Player2 _, _, Army Obliterated Obliterated Obliterated Obliterated Obliterated) ->
                                Nothing
                        (_, _, _) ->
                                Just bMod
        )


hasHealth : Maybe BattleModel -> Maybe BattleModel
hasHealth =
        Maybe.andThen (\bMod ->
            if bMod.p2.health > 0 && bMod.p1.health > 0 then
                Just bMod
            else
                Nothing
            )


{-| Because this takes place after turn reduction, some wonky
turn arithmetic is necessary until further notice. -}
validate : BattleModel -> Maybe BattleModel -> BattleState
validate bMod mb =
        case mb of
                Just b -> Battle b
                Nothing ->
                        case bMod.phase of
                                Player1 Second -> Player1Victory
                                Player2 First -> Player1Victory
                                Player2 Second -> Player2Victory
                                Player1 First -> Player2Victory


checkVictory bMod =
        let
                po b =
                        case b.phase of
                                Player1 _ ->
                                        .p2
                                Player2 _ ->
                                        .p1
        in
        bMod
        |> test
        |> hasUnits
        |> hasHealth
        |> validate bMod

type ShotResolution 
        = NoDamage
        | DamageTaken
        | TargetObliterated
        
samePieceHue piece1 piece2 =
        case (piece1, piece2) of
                (Piece Orange _, Piece Orange _) -> True
                (Piece Blue _, Piece Blue _) -> True
                (_, _) -> False

shotResolver : Piece -> Piece -> ShotResolution
shotResolver src target =
        case (target, samePieceHue src target) of
                (Obliterated, _) -> DamageTaken
                (Piece _ _, True) -> NoDamage
                (Piece _ _, False) -> TargetObliterated

--mapPlayer1Pieces fn bMod =
--        let
--            player = 

fireActivePiece spot bMod =
        let
                translatedSpot = 
                        case spot of 
                                1 -> 5
                                2 -> 4
                                3 -> 3
                                4 -> 2
                                5 -> 1
                                _ -> 1
                armyGetter p =
                            bMod
                            |> p
                            |> .army
                resolveShot src target =
                        shotResolver (armyGetter src |> getPieceAt spot) (armyGetter target |> getPieceAt translatedSpot)

                resolvedShot = 
                        case bMod.phase of 
                                Player1 _ ->
                                        resolveShot .p1 .p2
                                Player2 _ ->
                                        resolveShot .p2 .p1
                playerDamaged p =
                        { p | health = p.health - 1 }

                killPlayerPieceAt : Spot -> PlayerModel -> PlayerModel
                killPlayerPieceAt s p =
                        { p | army = killPieceAt s p.army }
        in
        case (resolvedShot, bMod.phase) of
                (DamageTaken, Player1 _) -> { bMod | p2 = playerDamaged bMod.p2}
                (DamageTaken, Player2 _) -> { bMod | p1 = playerDamaged bMod.p1}
                (NoDamage, _) -> bMod
                (TargetObliterated, Player1 _) -> { bMod | p2 = killPlayerPieceAt translatedSpot bMod.p2  }
                (TargetObliterated, Player2 _) -> { bMod | p1 = killPlayerPieceAt translatedSpot bMod.p1  }


                        

updateBattleState bAct bState =
        case (bState, bAct) of
                (Battle bMod, Switch spot1 spot2) ->
                        bMod
                        |> switchActivePieces spot1 spot2
                        |> reduceTurns
                        |> Battle
                (Battle bMod, Fire spot) ->
                        bMod
                        |> fireActivePiece spot
                        |> reduceTurns
                        |> checkVictory
                (Battle bMod, Forfeit) ->
                        bMod
                        |> victory
                (_, _) ->
                        bState

initBattleState =
        { p1 = Nothing
        , p2 = Nothing
        }
