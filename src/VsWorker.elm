module VsWorker exposing (main)

-- opponent coordination logic
main = Platform.worker 

type Player
        = Local Name PlayerModel
        | Remote Name Address PlayerModel
        | AI PlayerModel

type OpponentMode 
        = SingleDevice
        | RemoteConnection
        | Computer

type Model
        = Initialized OpponentMode
        | LocalSetup OpponentMode Name
        | PlayerConnected OpponentMode Name Name (Maybe Address) PlacementModel
        | GameReady Model
        | Player1Wait Model
        | Player2Wait Model
        | Closed


type alias ActiveModel =
        { history : List BattleAction
        , mode : CommunicationMode
        , battle : BattleModel
        }


update msg model =
        case (model, msg) of ->
                (Initialized mode, SetName n) ->
                        -- if mode == ai or single, start playing, connect ai, w/e
                        LocalSetup mode n
                (LocalSetup mode n, SetName n2) ->
                        PlayerConnected mode n n2 Nothing  { p1 = Nothing, p2 = Nothing } 
                (LocalSetup mode n, Connected n2 addr) ->
                        PlayerConnected mode n n2 (Just addr) { p1 = Nothing, p2 = Nothing }
                (PlayerConnected m n n2 ja pm, SetArmy a)



-- game logic

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


playerModel player =
        case player of
                Local _ pm -> pm
                Remote _ _ pm -> pm
                AI pm -> pm

playerType player =
        case player of
                Local _ _ -> Local
                Remote _ _ _ -> Remote
                AI _ -> AI

type Mood
        = Intrigued
        | Tense
        | Danger
        | Excited
        | Crushed
        | Triumphant

type alias PlacementModel =
        { p1 : Maybe PlayerModel
        , p2 : Maybe PlayerModel
        }

type alias BattleModel =
        { p1 : PlayerModel
        , p2 : PlayerModel
        , phase : BattlePhase
        } 

type BattleState
        = Placement PlacementModel
        | Battle BattleModel
        | Player1Victory
        | Player2Victory

type BattleAction
        = Switch Spot Spot
        | Fire Spot
        | Forfeit
        | PlacePlayer1 Army
        | PlacePlayer2 Army

type Turn
        = First
        | Second

type BattlePhase
        = Player1 Turn
        | Player2 Turn



getPieceAt spot (Army p1 p2 p3 p4 p5) =
        case spot of
                1 -> p1
                2 -> p2
                3 -> p3
                4 -> p4
                5 -> p5
                _ -> Obliterated

updatePieceAt spot newPiece (Army p1 p2 p3 p4 p5) =
        case spot of
                1 -> Army newPiece p2 p3 p4 p5
                2 -> Army p1 newPiece p3 p4 p5
                3 -> Army p1 p2 newPiece p4 p5
                4 -> Army p1 p2 p3 newPiece p5
                5 -> Army p1 p2 p3 p4 newPiece
                _ -> Army p1 p2 p3 p4 p5

killPieceAt spot =
        updatePieceAt spot Obliterated 

switchActivePieces spot1 spot2 ({p1, p2, phase} as bMod) =
        case phase of 
                Player1 _ -> 
                        { bMod | p1 = { p1 | army = updatePieceAt spot1 spot2 p1.army } }
                Player2 _ ->
                        { bMod | p2 = { p2 | army = updatePieceAt spot1 spot2 p2.army } }


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
                        _ -> 
                                identity


p1Model =
        .p1 >> playerModel
p2Model =
        .p2 >> playerModel

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
        Maybe.map (\bMod ->
                case (bMod.phase, bMod.p1.army, bMod.p2.army) of
                        (Player1 _, Army Obliterated Obliterated Obliterated Obliterated Obliterated, _) ->
                                Nothing
                        (Player2 _, _, Army Obliterated Obliterated Obliterated Obliterated Obliterated) ->
                                Nothing
                        (_, _, _) ->
                                Just bMod
        )


hasHealth : Maybe Player -> Maybe Player
hasHealth mp =
        Maybe.map (\bMod ->
                case (bMod.phase, bMod.p2.health > 0, bMod.p1.health > 0) of
                        (Player1 _, True, _) ->
                                Just bMod
                        (Player2 _, _, True) ->
                                Just bMod
                        (_, _, _) ->
                                Nothing
        )

validate : Maybe BattleModel -> BattleModel -> BattleState
validate mb bMod =
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

ready pMod =
        case (playerModel pMod.p1, playerModel pMod.p2) of
                (Just p1, Just p2) ->
                        Battle { p1 = p1, p2 = p2, phase = Player1 First }
                _ ->
                        Placement pMod

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
                armyGetter p =
                            bMod
                            |> p
                            |> playerModel
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
                (Placement pMod, PlacePlayer1 army) ->
                        { pMod | p1 = Just ((playerType pMod.p1) (PlayerModel army 5)) }
                        |> ready
                (Placement pMod, PlacePlayer2 army) ->
                        Placement { pMod | p2 = Just ((playerType pMod.p2) (PlayerModel army 5)) }
                        |> ready
                (_, _) ->
                        bState

initBattleState =
        { p1 = Nothing
        , p2 = Nothing
        }
