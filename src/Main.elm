module Main exposing (main)

import Svg exposing (Svg)
import Svg.Attributes
import Svg.Events
import Html exposing (Html)
import Html.Attributes
import Html.Events
import Array exposing (Array)
import Browser

type Color
        = Red
        | Yellow


type Honesty 
        = Truthful
        | Deceitful
        | Exposed

type Piece 
        = Obliterated
        | Active Color Honesty

type Army
        = Army Piece Piece Piece Piece Piece

type ShotResolution 
        = NoDamage
        | DamageTaken
        | TargetObliterated
        
sameColor piece1 piece2 =
        case (piece1, piece2) of
                (Active Red _, Active Red _) -> True
                (Active Yellow _, Active Yellow _) -> True
                (_, _) -> False

shotResolver : Piece -> Piece -> ShotResolution
shotResolver src target =
        case (target, sameColor src target) of
                (Obliterated, _) -> DamageTaken
                (Active _ _, True) -> NoDamage
                (Active _ _, False) -> TargetObliterated


colorString color honesty =
        case (color, honesty) of
                (Red, Deceitful) -> "yellow"
                (Yellow, Deceitful) -> "red"
                (Red, _) -> "red"
                (Yellow, _) -> "yellow"



type GameScene
        = Title
        | Options
        | Credits
        | Battle BattleModel
        | Tutorial TutorialPhase


type TurnNumber
        = First
        | Second

type TurnPhase
        = Turn TurnNumber PieceSelected
        | Evaluation TurnNumber
        | Complete

type BattlePhase
        = Placement (List Piece) (List Piece)
        | PlayerTurn TurnPhase 
        | OpponentTurn 
        | PassControl -- should have a model with list BattleMsg for replay mode
        | PlayerVictory
        | OpponentVictory

type alias PieceSelected = Maybe Int


type BattleMsg
        = SelectPiece Int
        | DeselectPiece
        | PlacePiece Int
        | MaskPiece Int Color
        | WatchRecap
        | SkipRecap
        | Terminate

type PlayMode
        = VsCPU
        | SingleDeviceVs
        
type alias BattleModel =
        { opponentPieces : Army
        , playerPieces : Army
        , opponentHealth : Int
        , playerHealth : Int
        , battlePhase : BattlePhase
        , playMode : PlayMode
        }


initialPlayerPieces =
        [ Active Red Truthful
        , Active Red Truthful
        , Active Red Truthful
        , Active Yellow Truthful
        , Active Yellow Truthful
        ]
        
initialOpponentPieces =
        [ Active Red Truthful
        , Active Red Truthful
        , Active Yellow Truthful
        , Active Yellow Truthful
        , Active Yellow Truthful
        ]

mapOpponentPieces fn gpm =
        { gpm | opponentPieces = fn gpm.opponentPieces }

mapOpponentHealth fn gpm =
        { gpm | opponentHealth = fn gpm.opponentHealth }

mapPlayerPieces fn gpm =
        { gpm | playerPieces = fn gpm.playerPieces }

mapTurnPhase fn gpm =
        { gpm | turnPhase = fn gpm.turnPhase }
        
type TutorialPhase
        = TeachingMoving
        | TeachingAttacking
        | TeachingColorChoice
        | TeachingEndGame

type alias Config =
        { displayName : String
        , permitUniqueColors : Bool
        , animations : Bool
        , sound : Bool
        }

initConfig : Config
initConfig =
        { displayName = ""
        , permitUniqueColors = True
        , animations = True
        , sound = True
        }


type alias Model =
        { gameScene : GameScene
        , config : Config
        , debugMode : Bool
        }

mapPieces fn (Army p1 p2 p3 p4 p5) =
        Army (fn p1) (fn p2) (fn p3) (fn p4) (fn p5)

mapIndexedPieces fn (Army p1 p2 p3 p4 p5) =
        Army (fn 1 p1) (fn 2 p2) (fn 3 p3) (fn 4 p4) (fn 5 p5)

armyPiecesList (Army p1 p2 p3 p4 p5) =
        [p1, p2, p3, p4, p5]

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

swapPiece oldSpot newSpot army =
        let
            activePiece = getPieceAt oldSpot army
            displacedPiece = getPieceAt newSpot army
        in
        army
        |> updatePieceAt newSpot activePiece
        |> updatePieceAt oldSpot displacedPiece

incrTurnNumber turnPhase =
        case turnPhase of
                Turn First _ -> Evaluation First
                Turn Second _ -> Evaluation Second
                other -> other



-- returns not bModel but gameScene with bModel???
updateBattleScene : BattleMsg -> BattleModel -> (GameScene, Cmd.Cmd Msg)
updateBattleScene bMsg bModel =
        let
                piecesSwapped oldSpot newSpot gpm =
                        { gpm | playerPieces = swapPiece oldSpot newSpot gpm.playerPieces }
                
                advanceTurn : BattleModel -> BattleModel
                advanceTurn gpm =
                        { gpm | battlePhase = case gpm.battlePhase of
                                PlayerTurn turnPhase ->
                                        case turnPhase of
                                               Turn First _ -> PlayerTurn (Turn Second Nothing)
                                               Turn Second _ -> if gpm.playMode == SingleDeviceVs then PassControl else OpponentTurn
                                               other -> PlayerTurn other

                                battlePhase -> battlePhase
                                }

                modelUpdate : BattleMsg -> BattleModel
                modelUpdate gpm = 
                        case gpm of
                                SelectPiece spot ->
                                        let
                                            playerPiece = getPieceAt spot bModel.playerPieces
                                            opponentPiece = getPieceAt spot bModel.opponentPieces
                                            resolveShot = 
                                                    case shotResolver playerPiece opponentPiece of
                                                        NoDamage -> 
                                                                identity
                                                        DamageTaken -> 
                                                                mapOpponentHealth ((+) -1)
                                                        TargetObliterated ->
                                                                mapOpponentPieces (killPieceAt spot)

                                        in 
                                        bModel 
                                        |> (\bn -> case bn.battlePhase of
                                                PlayerTurn (Turn _ (Just selectedSpot)) ->
                                                        if selectedSpot == spot then
                                                                bModel
                                                                |> resolveShot
                                                                |> advanceTurn
                                                                |> solveGame
                                                        else 
                                                                bModel
                                                                |> piecesSwapped selectedSpot spot
                                                                |> advanceTurn

                                                PlayerTurn (Turn turnNumber Nothing) ->
                                                        { bModel | battlePhase = PlayerTurn (Turn turnNumber (Just spot)) }
                                                _ ->
                                                        bModel
                                                )
                                                        --
                                DeselectPiece ->
                                        bModel
                                        
                                PlacePiece spot ->
                                        bModel
                                        -- if no pieces remain to place; advance battlephase
                                                --|> piecePlaced spot piece
                                MaskPiece spot _ ->
                                        bModel 
                                                --|> pieceLied spot
                                WatchRecap ->
                                        bModel
                                SkipRecap ->
                                        bModel
                                        |> (\bm ->
                                                let
                                                    newPArmy = bm.opponentPieces
                                                    newOArmy = bm.playerPieces
                                                in
                                                { bm | playerPieces = newPArmy
                                                        , opponentPieces = newOArmy
                                                }
                                                )
                                Terminate ->
                                        case bModel.battlePhase of
                                                Placement _ _ ->
                                                        { bModel | battlePhase = PlayerTurn (Turn First Nothing) }
                                                PlayerTurn _ ->
                                                        { bModel | battlePhase = PlayerVictory }
                                                OpponentTurn -> 
                                                        { bModel | battlePhase = OpponentVictory }
                                                _ ->
                                                        bModel
        in
        ( bMsg
                |> modelUpdate
                |> Battle
        , Cmd.none
        )

solveGame : BattleModel -> BattleModel
solveGame bModel =
        let
                winningConditionPiecesAnnihilated retriever gpm =
                        case gpm |> retriever of
                                Army Obliterated Obliterated Obliterated Obliterated Obliterated ->
                                        True
                                _ ->
                                        False
                winningConditionHealthDepleted retriever gpm =
                        retriever gpm <= 0

                winningConditionOpponentPiecesAnnihilated =
                        winningConditionPiecesAnnihilated .opponentPieces

                winningConditionPlayerPiecesAnnihilated =
                        winningConditionPiecesAnnihilated .playerPieces

                winningConditionOpponentHealthDepleted =
                        winningConditionHealthDepleted .opponentHealth

                winningConditionPlayerHealthDepleted =
                        winningConditionHealthDepleted .playerHealth

                opponentConditions =
                        [ winningConditionPlayerPiecesAnnihilated 
                        , winningConditionPlayerHealthDepleted
                        ]

                playerConditions = 
                        [ winningConditionOpponentPiecesAnnihilated 
                        , winningConditionOpponentHealthDepleted
                        ]

                rulesApply : (BattleModel -> Bool) -> Bool
                rulesApply fn =
                        fn bModel

                
        in
        case bModel.battlePhase of
                PlayerTurn (Evaluation turnNumber) ->
                        case (turnNumber, List.any rulesApply playerConditions) of
                                (_, True) -> { bModel | battlePhase = PlayerVictory }
                                (First, False) -> { bModel | battlePhase = PlayerTurn (Turn Second Nothing) }
                                (Second, False) -> { bModel | battlePhase = OpponentTurn }
                OpponentTurn ->
                        if List.any rulesApply opponentConditions then
                                { bModel | battlePhase = OpponentVictory }
                        else
                                { bModel | battlePhase = PlayerTurn (Turn First Nothing) }

                _ ->
                        bModel


type Msg
        = NoOp
        | BattleSceneMsg BattleMsg

type Menu
        = Menu (Html.Html Msg) (List MenuOption)

type alias MenuOption =
        { title : String
        , message : Msg
        , transformer : String -> Msg -> Html Msg
        }

-- gameplayscreen views

rematchView _ =
        [ Html.text "Would you like to have a rematch?" ]

awardingVictory _ =
        [ Html.text "The winner is ..." ]

pauseMenu _ =
        [ Html.text "Game paused: quit?" ]


-- tutorial views
teachingMoving _ =
        [ Html.text "You move a unit by selecting it and then selecting the piece to switch with it, or selecting an empty space for it to move to" ]

teachingAttacking _ =
        [ Html.text "You attack by selecting a piece and then selecting fire" ]

teachingColorChoice _ =
        [ Html.text "You can pick both the actual color and the displayed color of your pieces." ]

teachingEndGame _ =
        [ Html.text "You want to deal five blows to your opponent in order to win. Like colors block strikes, so aim for empty spots and dissimilar defenders." ]

-- shorthands
flatten =
        List.foldr (++) []

-- widget views
newMenuOption title message =
        { title = title
        , message = message
        , transformer = (\t _ -> Html.text t)
        }

menuOptions (Menu _ options) =
        options

menuTitle (Menu title _) =
        title


newMenu title options =
        Menu (Html.text title) options

menuOptionView menuOption =
        [ Html.button [] [ menuOption.transformer menuOption.title menuOption.message ] ]

menuView menu =
        (menu 
                |> menuTitle)
        :: (menu
                |> menuOptions
                |> List.map menuOptionView 
                |> flatten)
        

-- gamePhase views
mainMenuView _ =
        let 

                mainMenuTitleView =
                        Html.h1 [] [ Html.text "GAMENAME" ]
        in
        Menu mainMenuTitleView
                [ newMenuOption "New Game" NoOp
                , newMenuOption "Network" NoOp
                , newMenuOption "Tutorial" NoOp
                , newMenuOption "Options" NoOp
                , newMenuOption "Credits" NoOp
                ]
        |> menuView

optionsView _ =
        newMenu "Settings"
                [ { title = "Display Name", message = NoOp, transformer = (\_ _ -> Html.input [] []) } ] 




pieceFaceView spot piece = 
        case piece of 
                Obliterated -> 
                        Svg.text ""
                Active color honesty ->
                        Svg.rect 
                                [ Svg.Attributes.fill (colorString color honesty) 
                                , Svg.Attributes.x (spot * 12 |> String.fromInt)
                                , Svg.Attributes.y (0 |> String.fromInt)
                                , Svg.Attributes.width (10 |> String.fromInt)
                                , Svg.Attributes.height (10 |> String.fromInt)
                                ] []

pieceRearView spot piece =
        case piece of
                Obliterated -> 
                        Svg.text ""
                Active color honesty ->
                        Svg.rect 
                                [ Svg.Attributes.fill (colorString color Truthful) 
                                , Svg.Attributes.x (spot * 12 |> String.fromInt)
                                , Svg.Attributes.y (24 |> String.fromInt)
                                , Svg.Attributes.width (10 |> String.fromInt)
                                , Svg.Attributes.height (10 |> String.fromInt)
                                , Svg.Events.onClick (BattleSceneMsg (SelectPiece (spot + 1)))
                                ] []


playerTeamView : Army -> List (Svg Msg)
playerTeamView =
        armyPiecesList >> List.indexedMap pieceRearView
        --mapPieces pieceRearView

opponentTeamView : Army -> List (Svg Msg)
opponentTeamView =
        armyPiecesList >> List.indexedMap pieceFaceView


boardView : BattleModel -> Svg Msg 
boardView {playerPieces, opponentPieces, playerHealth, opponentHealth} =
        let
                hud =
                        [ --Svg.text_ (playerHealth |> String.fromInt) 
                        --, Svg.text_ (opponentHealth |> String.fromInt) 
                        ]
                pieces = 
                        [ Svg.g [] (playerTeamView playerPieces)
                        , Svg.g [] (opponentTeamView opponentPieces)
                        ] 

                
        in
        hud
        ++ pieces
        |> Svg.g []
        --++ grid
                


passControlView : BattleModel -> Svg Msg
passControlView bmodel =
        Svg.g
                []
                [ Svg.text_ [] [ Svg.text "Please pass control of device to next player" ]
                , Svg.rect
                        [ Svg.Attributes.fill "green"
                        , Svg.Attributes.x "50"
                        , Svg.Attributes.y "50"
                        , Svg.Attributes.width "20"
                        , Svg.Attributes.height "7"
                        , Svg.Events.onClick (BattleSceneMsg SkipRecap)
                        ]
                        [ Svg.text_ [ Svg.Attributes.fill "black" ] [ Svg.text "Continue" ] ]
                ]

subscriptions : Model -> Sub Msg
subscriptions _ =
        Sub.none

view : Model -> Browser.Document Msg
view model =
        let
                fn = 
                        case model.gameScene of
                                Battle gpm ->
                                        case gpm.battlePhase of
                                                PassControl ->
                                                        passControlView gpm
                                                _ ->
                                                        boardView gpm
                                _ -> Html.text ""
        in
        Browser.Document
                "GAME"
                ( 
                        [ Svg.svg 
                                [ Svg.Attributes.viewBox "0 0 100 100" ]
                                [ fn ]
                        ]
                        )

                

update msg model =
        case msg of
        BattleSceneMsg bMsg ->
                case model.gameScene of
                        Battle bModel ->
                                let
                                    (gScene, cmds) = updateBattleScene bMsg bModel
                                in
                                ( { model | gameScene = gScene }
                                , Cmd.batch [cmds, Cmd.none]
                                )
                        _ ->
                                ( model, Cmd.none )
        _ ->
                (model, Cmd.none)

defaultBattleModel : BattleModel
defaultBattleModel =
        { opponentPieces =
                Army 
                        (Active Red Truthful)
                        (Active Red Truthful)
                        (Active Red Truthful)
                        (Active Yellow Truthful)
                        (Active Yellow Truthful)
        , playerPieces =
                Army 
                        (Active Red Truthful)
                        (Active Red Truthful)
                        (Active Yellow Truthful)
                        (Active Yellow Truthful)
                        (Active Yellow Truthful)
        , opponentHealth = 5
        , playerHealth = 5
        , battlePhase = PlayerTurn (Turn First Nothing)
        , playMode = SingleDeviceVs 
        }

init : () -> ( Model, Cmd Msg)
init _ =
        (       { gameScene = Battle defaultBattleModel
                , config = initConfig
                , debugMode = True
                }
                
        , Cmd.none
        )

main = Browser.document 
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
