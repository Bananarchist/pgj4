module Logic exposing (Msg, decode, encode, BattleAction(..), Army(..), Piece(..), Color(..), Honesty(..), PlayerModel, initializeBattle, updatePieceAt, getPieceAt, updateHonestyAt, updateColorAt, initialArmy, Spot)

type alias Name = String
type alias Spot = Int
type alias Address = String

type Color
        = Orange
        | Blue

type Honesty
        = Authentic
        | Deceitful

type Piece 
        = Obliterated
        | Piece Color Honesty

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

decode = 
        Nothing

encode =
        Nothing

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

updateColorAt spot newColor (Army p1 p2 p3 p4 p5 as army) =
        let 
            piece = getPieceAt spot army
                |> (\p -> case p of
                        Piece _ h -> Piece newColor h
                        _ -> Piece newColor Authentic
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
                case (bMod.phase, bMod.p2.health > 0, bMod.p1.health > 0) of
                        (Player1 _, True, _) ->
                                Just bMod
                        (Player2 _, _, True) ->
                                Just bMod
                        (_, _, _) ->
                                Nothing
        )

validate : BattleModel -> Maybe BattleModel -> BattleState
validate bMod mb =
        case mb of
                Just b -> Battle b
                Nothing ->
                        case bMod.phase of
                                Player1 _ -> Player1Victory
                                Player2 _ -> Player2Victory


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
        
samePieceColor piece1 piece2 =
        case (piece1, piece2) of
                (Piece Orange _, Piece Orange _) -> True
                (Piece Blue _, Piece Blue _) -> True
                (_, _) -> False

shotResolver : Piece -> Piece -> ShotResolution
shotResolver src target =
        case (target, samePieceColor src target) of
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
                (TargetObliterated, Player1 _) -> { bMod | p2 = killPlayerPieceAt spot bMod.p2  }
                (TargetObliterated, Player2 _) -> { bMod | p1 = killPlayerPieceAt spot bMod.p1  }


                        

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
