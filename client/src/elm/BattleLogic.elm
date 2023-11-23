module BattleLogic exposing (..)

import Basics.Extra exposing (flip)
import U

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

initialArmy : Army
initialArmy = 
        Army Obliterated Obliterated Obliterated Obliterated Obliterated

blueArmy : Army
blueArmy =
    Army 
        (Piece Blue Authentic ) 
        (Piece Blue Authentic ) 
        (Piece Orange Deceitful ) 
        (Piece Blue Authentic ) 
        (Piece Orange Deceitful )


orangeArmy : Army
orangeArmy =
    Army 
        (Piece Orange Deceitful ) 
        (Piece Orange Deceitful ) 
        (Piece Orange Authentic ) 
        (Piece Orange Authentic )
        (Piece Orange Deceitful )


type alias PlayerModel = 
        { army : Army
        , health : Int
        }

initialPlayerModel : Army -> PlayerModel
initialPlayerModel army =
    PlayerModel army 5

type alias BattleModel =
        { p1 : PlayerModel
        , p2 : PlayerModel
        , phase : BattlePhase
        } 

init : PlayerModel -> PlayerModel -> BattleModel
init p1m p2m =
    BattleModel p1m p2m (Player1 First)

type alias History = (List TurnRecap, BattleModel)

type BattleState
        = Battle BattleModel History
        | Player1Victory History
        | Player2Victory History

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

type TurnRecap 
    = SuccessfulShot Spot
    | UnsuccessfulShot Spot
    | Damaged Spot
    | SwappedPieces Spot Spot
    | MovedToEmptySpot Spot Spot
    | TurnEnded
    | GameEnded

recap bState =
    case bState of
        Battle _ h -> h
        Player1Victory h -> h
        Player2Victory h -> h

player1Health bState =
    case bState of 
        Battle { p1 } _ ->
            p1.health
        _ ->
            0
player2Health bState =
    case bState of 
        Battle { p2 } _ ->
            p2.health
        _ ->
            0

player1Army bState =
    case bState of 
        Battle { p1 } _ ->
            p1.army
        _ ->
            initialArmy
player2Army bState =
    case bState of 
        Battle { p2 } _ ->
            p2.army
        _ ->
            initialArmy
decode = 
        Nothing

encode =
        Nothing

player1Turnか : BattleState -> Bool
player1Turnか =
    currentTurn >> U.equalsAnyOf [Player1 First, Player1 Second]

currentTurn : BattleState -> BattlePhase
currentTurn bState =
    case bState of
        Battle { phase } _ ->
            phase
        _ ->
            Player1 First

player1TurnStart : BattleState -> Bool
player1TurnStart bState =
    case bState of
        Battle { phase } _ ->
            case phase of
                Player1 First -> True
                _ -> False
        _ -> False


player2TurnStart : BattleState -> Bool
player2TurnStart bState =
    case bState of
        Battle { phase } _ ->
            case phase of
                Player2 First -> True
                _ -> False
        _ -> False
    

updatePlayer1Army army bState = 
    case bState of 
        Battle ({ p1 } as bMod) h ->
            Battle { bMod | p1 = { p1 | army = army } } h
        _ ->
            bState

updatePlayer2Army army bState = 
    case bState of 
        Battle ({ p2 } as bMod) h ->
            Battle { bMod | p2 = { p2 | army = army } } h
        _ ->
            bState

mapPlayer1Army fn bState =
    case bState of
        Battle ({ p1 } as bMod) h ->
            Battle { bMod | p1 = { p1 | army = fn bMod } } h
        _ ->
            bState

mapPlayer2Army fn bState =
    case bState of
        Battle ({ p2 } as bMod) h ->
            Battle { bMod | p2 = { p2 | army = fn bMod } } h
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
        topOrOb = List.head >> Maybe.withDefault Obliterated
        cl = (List.length l) |> (-) 5 |> flip List.repeat Obliterated |> (++) l
        p1 = List.head cl |> Maybe.withDefault Obliterated
        p2 = List.drop 1 cl |> topOrOb
        p3 = List.drop 2 cl |> topOrOb
        p4 = List.drop 3 cl |> topOrOb
        p5 = List.drop 4 cl |> topOrOb
    in
    Army p1 p2 p3 p4 p5

authenticか : Piece -> Bool
authenticか piece =
    case piece of
        Piece _ Deceitful -> False
        _ -> True

getHueOf : Bool -> Piece -> Maybe Hue
getHueOf trueColor piece =
    case piece of 
        Piece h Authentic -> Just h
        Piece h Deceitful -> if trueColor then Just h else Just (oppositeHue h)
        _ -> Nothing

getHueAt : Bool -> Int -> Army -> Maybe Hue
getHueAt trueColor spot army =
    let piece = getPieceAt spot army
    in getHueOf trueColor piece

hueString : Hue -> String
hueString h =
    case h of
        Blue -> "Blue"
        Orange -> "Orange"

oppositeHue : Hue -> Hue
oppositeHue h =
    case h of
        Blue -> Orange
        Orange -> Blue

getHonestyOf : Piece -> Maybe Honesty
getHonestyOf piece =
    case piece of
        Piece _ h -> Just h
        _ -> Nothing


getHonestyAt : Int -> Army -> Maybe Honesty
getHonestyAt spot army =
    let piece = getPieceAt spot army
    in getHonestyOf piece

honestyString : Honesty -> String
honestyString honesty =
    case honesty of
        Authentic -> "Authentic"
        Deceitful -> "Deceitful"

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

switchActivePieces : Spot -> Spot -> BattleModel -> Result () (List TurnRecap, BattleModel)
switchActivePieces spot1 spot2 ({p1, p2, phase} as bMod) =
    let
        pGetter = 
            case phase of 
                Player1 _ -> .p1
                Player2 _ -> .p2
        army = (pGetter >> .army) bMod
        srcPiece = getPieceAt spot1 army
        destPiece = getPieceAt spot2 army
        swapped =
            case (srcPiece, destPiece) of
                (Obliterated, _) -> False
                (_, Obliterated) -> False
                (_, _) -> True
        rcp = if swapped then [ SwappedPieces spot1 spot2 ] else [ MovedToEmptySpot spot1 spot2 ]
    in
    if spot1 > 5 || spot1 < 1 || spot2 > 5 || spot2 < 0 then
        Err ()
    else case phase of 
        Player1 _ -> 
            ( rcp
            , { bMod | p1 = { p1 | army = updatePieceAt spot2 (srcPiece) (updatePieceAt spot1 (destPiece) p1.army) } } 
            )
                |> Ok
        Player2 _ ->
            ( rcp
            , { bMod | p2 = { p1 | army = updatePieceAt spot2 (srcPiece) (updatePieceAt spot1 (destPiece) p2.army) } }
            )
            |> Ok


modelWithPhase : BattlePhase -> BattleModel -> BattleModel
modelWithPhase p bMod =
        { bMod | phase = p }

reduceTurns battle =
    case battle of
        Battle bMod h ->
            case bMod.phase of
                Player1 First -> 
                        modelWithPhase (Player1 Second) bMod
                        |> flip Battle h
                
                Player1 Second -> 
                        modelWithPhase (Player2 First) bMod
                        |> Tuple.pair [ TurnEnded ]
                        |> updateRecap h

                Player2 First -> 
                        modelWithPhase (Player2 Second) bMod
                        |> flip Battle h

                Player2 Second -> 
                        modelWithPhase (Player1 First) bMod
                        |> Tuple.pair [ TurnEnded ]
                        |> updateRecap h
        _ -> battle


p1Model =
        .p1 
p2Model =
        .p2

forfeit bMod =
        case bMod.phase of
                Player1 _ ->
                        Player2Victory ([], bMod)
                Player2 _ ->
                        Player1Victory ([], bMod)

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
validate : BattleModel -> History -> Maybe BattleModel -> BattleState
validate bMod history mb =
    case mb of
        Just b -> Battle b history
        Nothing ->
            case bMod.phase of
                Player1 Second -> Player1Victory history 
                Player2 First -> Player1Victory history
                Player2 Second -> Player2Victory history
                Player1 First -> Player2Victory history


checkVictory bState =
    case bState of
        Battle bMod history ->
            bMod
            |> test
            |> hasUnits
            |> hasHealth
            |> validate bMod history
        _ -> bState

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

translatedSpot : Spot -> Spot
translatedSpot spot = 
        case spot of 
                1 -> 5
                2 -> 4
                3 -> 3
                4 -> 2
                5 -> 1
                _ -> 1

fireActivePiece spot bMod =
    let
        armyGetter p =
            bMod
            |> p
            |> .army

        resolveShot src target =
            shotResolver (armyGetter src |> getPieceAt spot) (armyGetter target |> getPieceAt spot)

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

        makeShooterHonestAt : Spot -> PlayerModel -> PlayerModel
        makeShooterHonestAt s p =
            { p | army = updateHonestyAt s Authentic p.army }
    in
    case (resolvedShot, bMod.phase) of
        (DamageTaken, Player1 _) -> 
            ( [ Damaged spot ]
            , { bMod | p2 = playerDamaged bMod.p2, p1 = makeShooterHonestAt spot bMod.p1 } 
            )
        (DamageTaken, Player2 _) -> 
            ( [ Damaged spot ]
            , { bMod | p1 = playerDamaged bMod.p1, p2 = makeShooterHonestAt spot bMod.p2 } 
            )
        (NoDamage, Player1 _) -> 
            ( [ UnsuccessfulShot spot ]
            , { bMod | p1 = makeShooterHonestAt spot bMod.p1 }
            )
        (NoDamage, Player2 _) -> 
            ( [ UnsuccessfulShot spot ]
            , { bMod | p2 = makeShooterHonestAt spot bMod.p2 } 
            )
        (TargetObliterated, Player1 _) -> 
            ( [ SuccessfulShot spot ]
            , { bMod 
                | p2 = killPlayerPieceAt spot bMod.p2, p1 = makeShooterHonestAt spot bMod.p1 } 
            )
        (TargetObliterated, Player2 _) -> 
            ( [ SuccessfulShot spot]
            , { bMod 
                | p1 = killPlayerPieceAt spot bMod.p1, p2 = makeShooterHonestAt spot bMod.p2 } 
            )


updateRecap : History -> (List TurnRecap, BattleModel) -> BattleState
updateRecap (oldEvents, initial) (newEvents, bm) =
    Battle bm (newEvents ++ oldEvents, initial)

update bAct bState =
        case (bState, bAct) of
                (Battle bMod h, Switch spot1 spot2) ->
                        bMod
                        |> switchActivePieces spot1 spot2
                        |> Result.map
                            ( updateRecap h 
                            >> reduceTurns 
                            )
                        |> Result.withDefault bState
                (Battle bMod h, Fire spot) ->
                        bMod
                        |> fireActivePiece spot
                        |> updateRecap h
                        |> reduceTurns
                        |> checkVictory
                (Battle bMod h, Forfeit) ->
                        bMod
                        |> forfeit
                (_, _) ->
                        bState




