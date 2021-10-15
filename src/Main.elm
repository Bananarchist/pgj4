module Main exposing (main)

import Svg exposing (Svg)
import Svg.Attributes
import Svg.Events
import Html exposing (Html)
import Html.Attributes
import Html.Events
import Array exposing (Array)
import Browser
import Browser.Events
import Json.Decode as Decode
import Point2d
import Rectangle3d
import SketchPlane3d
import Task
import Rectangle2d
import Vector3d
import Browser.Dom
import Angle
import Scene3d.Light
import Luminance
import Illuminance
import LuminousFlux
import Axis3d
import Scene3d
import Point3d
import Point3d.Projection
import Color
import Block3d
import Scene3d.Material as Material
import Camera3d
import Viewpoint3d
import Direction3d
import Angle
import Length
import Pixels

type PieceColor
        = Orange
        | Blue


type Honesty 
        = Authentic
        | Deceitful

type Piece 
        = Obliterated
        | Active PieceColor Honesty

type Army
        = Army Piece Piece Piece Piece Piece

type ShotResolution 
        = NoDamage
        | DamageTaken
        | TargetObliterated
        
samePieceColor piece1 piece2 =
        case (piece1, piece2) of
                (Active Orange _, Active Orange _) -> True
                (Active Blue _, Active Blue _) -> True
                (_, _) -> False

shotResolver : Piece -> Piece -> ShotResolution
shotResolver src target =
        case (target, samePieceColor src target) of
                (Obliterated, _) -> DamageTaken
                (Active _ _, True) -> NoDamage
                (Active _ _, False) -> TargetObliterated


colorString color honesty =
        case (color, honesty) of
                (Orange, Deceitful) -> "blue"
                (Blue, Deceitful) -> "orange"
                (Orange, _) -> "orange"
                (Blue, _) -> "blue"



type GameScene
        = Title
        | GameConfig GameConfigModel
        | Options
        | Credits
        | Battle BattleModel
        | Tutorial TutorialPhase


type GameConfigModel
        = NothingConfigured
        | AIGameConfigured
        | SDVChosen String
        | SDVChosenOneName String String
        | SDVConfigured String String

type GameConfigMsg 
        = SetPlayModeAI
        | SetPlayModeSingleDevice
        | SetPlayer1Name String
        | SetPlayer2Name String
        | ConfirmConfig
        | GoBack

defaultGameConfigModel : GameConfigModel
defaultGameConfigModel =
        NothingConfigured



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
        | MaskPiece Int PieceColor
        | WatchRecap
        | SkipRecap
        | Terminate

type PlayMode
        = VsCPU
        | SingleDeviceVs
        
type alias BattleModel =
        { player1 : PlayerModel
        , player2 : PlayerModel
        , battlePhase : BattlePhase
        , playMode : PlayMode
        }


initialPlayerPieces =
        [ Active Orange Authentic
        , Active Orange Authentic
        , Active Orange Authentic
        , Active Blue Authentic
        , Active Blue Authentic
        ]
        
initialOpponentPieces =
        [ Active Orange Authentic
        , Active Orange Authentic
        , Active Blue Authentic
        , Active Blue Authentic
        , Active Blue Authentic
        ]


mapOpponentPieces fn gpm =
        let
            oldp2 = gpm.player2
            newp2 = { oldp2 | army = fn oldp2.army }
        in
        { gpm | player2 = newp2 }



mapOpponentHealth fn gpm =
        let
            oldp2 = gpm.player2
            newp2 = { oldp2 | health = fn oldp2.health }
        in
        { gpm | player2 = newp2 }


mapPlayerPieces fn gpm =
        let
            oldp1 = gpm.player1
            newp1 = { oldp1 | army = fn oldp1.army }
        in
        { gpm | player1 = newp1 }

mapTurnPhase fn gpm =
        { gpm | turnPhase = fn gpm.turnPhase }
        
type TutorialPhase
        = TeachingMoving
        | TeachingAttacking
        | TeachingPieceColorChoice
        | TeachingEndGame

type alias Config =
        { displayName : String
        , permitUniquePieceColors : Bool
        , animations : Bool
        , sound : Bool
        }

initConfig : Config
initConfig =
        { displayName = ""
        , permitUniquePieceColors = True
        , animations = True
        , sound = True
        }


type alias Model =
        { gameScene : GameScene
        , config : Config
        , debugMode : Bool
        , width : Int
        , height : Int
        , helpOverlay : Bool
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
                        let 
                                player1 = gpm.player1
                                newPlayer1 = { player1
                                        | army = swapPiece oldSpot newSpot player1.army }
                                
                        in
                        { gpm | player1 = newPlayer1 }
                
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
                                            playerPiece = getPieceAt spot bModel.player1.army
                                            opponentPiece = getPieceAt spot bModel.player2.army
                                            resolveShot : (BattleModel -> BattleModel)
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
                                                    newO = bm.player2
                                                    newP = bm.player1
                                                in
                                                { bm | player1 = newO
                                                        , player2 = newP
                                                        , battlePhase = PlayerTurn (Turn First Nothing)
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
                winningConditionPiecesAnnihilated player gpm =
                        case (player >> .army) gpm of
                                Army Obliterated Obliterated Obliterated Obliterated Obliterated ->
                                        True
                                _ ->
                                        False
                winningConditionHealthDepleted player gpm =
                        (player >> .health) gpm <= 0

                winningConditionOpponentPiecesAnnihilated =
                        winningConditionPiecesAnnihilated .player2

                winningConditionPlayerPiecesAnnihilated =
                        winningConditionPiecesAnnihilated .player1

                winningConditionOpponentHealthDepleted =
                        winningConditionHealthDepleted .player2

                winningConditionPlayerHealthDepleted =
                        winningConditionHealthDepleted .player1

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


type GameActions 
        = FireOn Int
        | MoveTo Int


type TitleMenuMsg 
        = ViewCredits
        | ViewOptions
        | StartLocalGame (Maybe PlayMode)
        | ViewTutorial

type Msg
        = NoOp
        | SetWindowSize Int Int 
        | Click Int 
        | KeyPress String
        | BattleSceneMsg BattleMsg
        | TitleMenuSceneMsg TitleMenuMsg
        | GameConfigSceneMsg GameConfigMsg

type Menu
        = Menu (List MenuOption)
        | Confirmation
        | ArmyList

type alias MenuOption = String

optionsForMenu menu =
        case menu of
                Menu opts -> opts
                Confirmation -> ["Confirm", "Cancel"]
                ArmyList -> ["1", "2", "3", "4", "5"]

titleMenu = Menu ["Start Game", "Tutorial", "Options", "Credits"]
battleMenu = ArmyList

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

teachingPieceColorChoice _ =
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


-- gamePhase views
type Row
        = Back
        | Front


entityView row spot piece =
        let
            cubeSize = 2
        in
        case piece of
                Obliterated ->
                        Scene3d.group []
                Active color honesty ->
                        let
                                (frontColor, backColor) = 
                                        case (color, honesty) of
                                                (Orange, Authentic) ->
                                                        (Color.orange, Color.orange)
                                                (Orange, Deceitful) ->
                                                        (Color.blue, Color.orange)
                                                (Blue, Authentic) ->
                                                        (Color.blue, Color.blue)
                                                (Blue, Deceitful) ->
                                                        (Color.orange, Color.blue)
                                        
                        in
                        Scene3d.group
                                [ Scene3d.quad (Material.metal { baseColor = Color.darkCharcoal, roughness = 1.0 })
                                    (Point3d.meters -1 -1 -1)
                                    (Point3d.meters 1 -1 -1)
                                    (Point3d.meters 1 1 -1)
                                    (Point3d.meters -1 1 -1)
                                   -- face away
                                , Scene3d.quad (Material.emissive (Scene3d.Light.color frontColor) (Luminance.nits 100)) 
                                    (Point3d.meters -1 -1 -1)
                                    (Point3d.meters -1 -1 1)
                                    (Point3d.meters -1 1 1)
                                    (Point3d.meters -1 1 -1)
                                    --top view
                                , Scene3d.quad (Material.emissive (Scene3d.Light.color frontColor) (Luminance.nits 100)) 
                                    (Point3d.meters -1 -1 1)
                                    (Point3d.meters 1 -1 1)
                                    (Point3d.meters 1 1 1)
                                    (Point3d.meters -1 1 1)
                                    -- left facing
                                , Scene3d.quad (Material.metal { baseColor = Color.darkCharcoal, roughness = 1.0 })
                                    (Point3d.meters -1 -1 -1)
                                    (Point3d.meters -1 -1 1)
                                    (Point3d.meters 1 -1 1)
                                    (Point3d.meters 1 -1 -1)
                                    --slightly right
                                , Scene3d.quad (Material.metal { baseColor = Color.darkCharcoal, roughness = 1.0 })
                                    (Point3d.meters 1 1 -1)
                                    (Point3d.meters -1 1 -1)
                                    (Point3d.meters -1 1 1)
                                    (Point3d.meters 1 1 1)
                                    --face viewer
                                , Scene3d.quad (Material.emissive (Scene3d.Light.color backColor) (Luminance.nits 100)) 
                                    (Point3d.meters 1 -1 1)
                                    (Point3d.meters 1 -1 -1)
                                    (Point3d.meters 1 1 -1)
                                    (Point3d.meters 1 1 1)

                                ]
                                |> p2faceoff row
                                |> spotTranslation spot
                                |> battleSpace row
p2faceoff row =
        case row of
                Back ->
                        Scene3d.rotateAround Axis3d.z (Angle.degrees 180)
                _ -> identity

battleSpace row =
        case row of
                Back -> 
                        Scene3d.translateBy
                                (Vector3d.meters -8.7 0 0)
                _ ->
                        Scene3d.translateBy
                                (Vector3d.meters 0.8 0 0)

spotTranslation : Int -> Scene3d.Entity coordinates -> Scene3d.Entity coordinates
spotTranslation spot =
        Scene3d.translateBy
                (Vector3d.meters 0 (spot - 2 |> toFloat |> (*) 2.1) 0)


armyRearView = 
        armyPiecesList >> List.indexedMap (entityView Back) 

armyFrontView =
        armyPiecesList >> List.indexedMap (entityView Front)

menuArmy count = 
        case count of 
                1 -> Army (Active Orange Authentic) Obliterated Obliterated Obliterated Obliterated
                2 -> Army (Active Orange Authentic) (Active Orange Authentic) Obliterated Obliterated Obliterated
                3 -> Army (Active Orange Authentic) (Active Orange Authentic) (Active Orange Authentic) Obliterated Obliterated
                4 -> Army (Active Orange Authentic) (Active Orange Authentic) (Active Orange Authentic) (Active Orange Authentic) Obliterated
                5 -> Army (Active Orange Authentic) (Active Orange Authentic) (Active Orange Authentic) (Active Orange Authentic) (Active Orange Authentic) 
                _ -> Army Obliterated Obliterated Obliterated Obliterated Obliterated

menuView width height menu =
        let 
                options = optionsForMenu menu
                totalOptions = List.length options
                pieces = 
                        totalOptions
                        |> menuArmy
                        |> armyFrontView
                hv = (healthView totalOptions Front)
                mainLight =
                        Scene3d.Light.point (Scene3d.Light.castsShadows True)
                                { chromaticity = Scene3d.Light.incandescent
                                , intensity = LuminousFlux.lumens 10000
                                , position = Point3d.meters 0 0 4
                                }
                text = 
                        Svg.svg 
                                [ Svg.Attributes.viewBox ("0 0 " ++ (String.fromInt width) ++ " " ++ (String.fromInt height))
                                , Svg.Attributes.width (String.fromInt width)
                                , Svg.Attributes.height (String.fromInt height)
                                ]
                                (List.indexedMap (\i o -> scene3dText o i (toFloat width) (toFloat height)) options)
        in
        [ text
        , Scene3d.custom
                { entities = hv ++ pieces
                , camera = camera
                , clipDepth = Length.meters 1
                , background = Scene3d.backgroundColor Color.black
                , dimensions = ( Pixels.pixels width, Pixels.pixels height )
                , lights = Scene3d.oneLight mainLight
                , exposure = Scene3d.exposureValue 8 
                , toneMapping = Scene3d.noToneMapping
                , whiteBalance = Scene3d.Light.daylight
                , antialiasing = Scene3d.multisampling
                }
        ]



battleView width height {player1, player2} =
        let
                pieces = armyFrontView player1.army
                opp = armyRearView player2.army
                hv = (healthView player2.health Back)
                        ++ (healthView player1.health Front)
                mainLight =
                        Scene3d.Light.point (Scene3d.Light.castsShadows True)
                                { chromaticity = Scene3d.Light.incandescent
                                , intensity = LuminousFlux.lumens 10000
                                , position = Point3d.meters 0 0 4
                                }
                softLight =
                        Scene3d.Light.soft 
                                { upDirection = Direction3d.positiveZ
                                , chromaticity = Scene3d.Light.fluorescent
                                , intensityAbove = Illuminance.lux 400
                                , intensityBelow = Illuminance.lux 100
                                }
        in
        Scene3d.custom
                { entities = hv ++ pieces ++ opp
                , camera = camera
                , clipDepth = Length.meters 1
                , background = Scene3d.backgroundColor Color.black
                , dimensions = ( Pixels.pixels width, Pixels.pixels height )
                , lights = Scene3d.oneLight mainLight
                , exposure = Scene3d.exposureValue 8 
                , toneMapping = Scene3d.noToneMapping
                , whiteBalance = Scene3d.Light.daylight
                , antialiasing = Scene3d.multisampling
                }

scene3dText string spot width height =
        let
                --spot = 5 - s
                screenRect = 
                        Rectangle2d.from Point2d.origin (Point2d.pixels width height)

                                
                spotRect3d =
                        Rectangle3d.on SketchPlane3d.yz
                                <| Rectangle2d.from 
                                        (Point2d.meters -1 9)
                                        (Point2d.meters 1 -1)
                rect =
                        [ (Point3d.meters -1 -1 -1)
                        , (Point3d.meters -1 -1 9)
                        , (Point3d.meters -1 1 9)
                        , (Point3d.meters -1 1 -1) ]
                        |> List.map (
                                Point3d.translateBy (Vector3d.meters 0 (spot - 2 |> toFloat |> (*) 2.1) 0)
                                >> Point3d.translateBy (Vector3d.meters -11 0 0)
                                >> Point3d.Projection.toScreenSpace camera screenRect)

                vertex = 
                        case rect of 
                                a :: bcd -> a
                                [] -> Point2d.origin
                xcoord = Point2d.xCoordinate vertex |> Pixels.toFloat
                ycoord = Point2d.yCoordinate vertex |> Pixels.toFloat
                x = String.fromFloat xcoord
                y = String.fromFloat ycoord
        in
        Svg.text_ 
                [ Svg.Attributes.fill "red"
                , Svg.Attributes.x x
                , Svg.Attributes.y y
                , Svg.Attributes.transform ("rotate(-90 " ++ x ++ " " ++ y ++");")
                ] 
                [ Svg.text string ]

camera = 
        Camera3d.perspective 
                { viewpoint = 
                        Viewpoint3d.lookAt
                                { eyePoint = Point3d.meters 10.5 4 4 
                                , focalPoint = Point3d.meters -1 0 1.5 
                                , upDirection = Direction3d.positiveZ
                }
                , verticalFieldOfView = Angle.degrees 40
                }

healthBlock fn =
        fn
                (Point3d.meters -1 -1 -1)
                (Point3d.meters -1 -1 9)
                (Point3d.meters -1 1 9)
                (Point3d.meters -1 1 -1)

healthView total facing =
        let
                healthy = 
                        Scene3d.quad (Material.emissive Scene3d.Light.daylight (Luminance.nits 300))
                dead =
                        Scene3d.quad (Material.color Color.black)
                --mapFn : Int -> Scene3d.Entity coordinates -> Scene3d.Entity coordinates
                mapFn index entities =
                        spotTranslation index entities |> faceHealthIndicators facing
        in
        List.repeat (5-total) (healthBlock dead)
        ++ List.repeat total (healthBlock healthy)
        |> List.indexedMap (mapFn)
                

faceHealthIndicators : Row -> Scene3d.Entity coordinates -> Scene3d.Entity coordinates
faceHealthIndicators facing =
        case facing of
                Front -> Scene3d.translateBy (Vector3d.meters -11 0 0)
                Back -> Scene3d.rotateAround Axis3d.y (Angle.degrees 90)
                        >> Scene3d.translateBy (Vector3d.meters 0.8 0 -2)


passControlView : BattleModel -> Svg Msg
passControlView bmodel =
        Html.article [ Html.Attributes.id "pass-control-view" ]
                [ Html.p [] [ Html.text "Please pass control of device to next player" ]
                , Html.button [ Html.Events.onClick (BattleSceneMsg WatchRecap) ] [ Html.text "Watch recap" ]
                , Html.button [ Html.Events.onClick (BattleSceneMsg SkipRecap) ] [ Html.text "Skip >" ]
                ]


viewBoxFor width height =
        Svg.Attributes.viewBox ("0 0 " ++ (String.fromInt width) ++ " " ++ (String.fromInt height))

fullScreenSvgAttrs width height =
        [ Svg.Attributes.viewBox ("0 0 " ++ (String.fromInt width) ++ " " ++ (String.fromInt height))
        , Svg.Attributes.width (String.fromInt width)
        , Svg.Attributes.height (String.fromInt height)
        ]

helpOverlayView {gameScene, width, height } =
        let
                xformula totalOptions spot = width // totalOptions * spot
                textXFormula totalOptions spot = width // (totalOptions * 2) * (spot * 2 + 1)
        in
        case gameScene of 
                Title ->
                        let
                                opts = optionsForMenu titleMenu
                                titleX = xformula (List.length opts)
                                textX = textXFormula (List.length opts)
                        in
                        Svg.svg
                                (Svg.Attributes.class "help-overlay" :: fullScreenSvgAttrs width height)
                                (List.indexedMap (\spot label ->
                                        Svg.g [] [
                                                Svg.line
                                                        [ Svg.Attributes.y1 "0"
                                                        , Svg.Attributes.y2 (String.fromInt height)
                                                        , Svg.Attributes.x1 (titleX spot |> String.fromInt)
                                                        , Svg.Attributes.x2 (titleX spot |> String.fromInt)
                                                        , Svg.Attributes.stroke "green"
                                                        ] []
                                                , Svg.text_ 
                                                        [ Svg.Attributes.x (textX spot |> String.fromInt)
                                                        , Svg.Attributes.y (height // 2 - 10 |> String.fromInt)
                                                        , Svg.Attributes.strokeDasharray "4"
                                                        , Svg.Attributes.fill "green"
                                                        ] [ Svg.text ((String.fromInt spot) ++ " " ++ label) ]
                                                ]
                                        ) opts
                                )
                _ ->
                        Svg.svg [] []

                        

gameConfigView : GameConfigModel -> Int -> Int -> List (Html Msg)
gameConfigView gcModel width height=
        let 
                cancelButton = 
                        [ Html.button [ Html.Events.onClick (GameConfigSceneMsg GoBack) ] [ Html.text "Cancel" ] ]
                confirmButton = 
                        [ Html.button [ Html.Events.onClick (GameConfigSceneMsg ConfirmConfig) ] [ Html.text "Confirm" ] ]
                nameInput msg val = 
                        [ Html.input [ Html.Events.onInput msg ] [ ]
                        ]
        in
        case gcModel of 
                NothingConfigured ->
                        menuView width height (Menu ["AI Battle", "2P", "Cancel"])
                AIGameConfigured ->
                        menuView width height (Menu ["Confirm - AI Game", "Cancel"])
                SDVChosen name ->
                        [ Html.section [ Html.Attributes.class "modal" ]
                                (Html.h1 [] [ Html.text "Enter Your Name" ]
                                :: nameInput (GameConfigSceneMsg << SetPlayer1Name) name
                        )--, menuView...
                        ]
                SDVChosenOneName _ name ->
                        Html.h1 [] [ Html.text "Enter Your Opponent's Name" ]
                        :: nameInput (GameConfigSceneMsg << SetPlayer2Name) name
                        ++ confirmButton
                        ++ cancelButton
                SDVConfigured _ _ ->
                        [ Html.h1 [] [ Html.text "Starting your match..." ] ]


keyDecoder : Decode.Decoder Msg
keyDecoder =
        Decode.map KeyPress
                (Decode.field "key" Decode.string)


mouseDecoder : Decode.Decoder Msg
mouseDecoder =
        Decode.map Click
                (Decode.field "pageX" Decode.int)

subscriptions : Model -> Sub Msg
subscriptions _ =
         Sub.batch
                 [ Browser.Events.onResize SetWindowSize
                 , Browser.Events.onClick (mouseDecoder)
                 , Browser.Events.onKeyPress (keyDecoder) 

                 ]

view : Model -> Html Msg
view model =
        let
                fn : List (Html Msg)
                fn = 
                        case model.gameScene of
                                Battle gpm ->
                                        case gpm.battlePhase of
                                                PassControl ->
                                                        [ passControlView gpm ]
                                                _ ->
                                                        [ battleView model.width model.height gpm ]
                                                        {-[ Svg.svg 
                                                                [ Svg.Attributes.viewBox "0 0 100 100" ]
                                                                [ boardView gpm ]
                                                                ]-}
                                Title ->
                                        menuView model.width model.height titleMenu
                                GameConfig gcm ->
                                        gameConfigView gcm model.width model.height

                                _ -> [ Html.text "" ]
                els = 
                        if model.helpOverlay then
                                helpOverlayView model :: fn
                        else
                                fn
        in
        Html.main_ [] els

                

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
        let
                sansCmd m = (m, Cmd.none)
                withGameScene g m = {m | gameScene = g }
                sceneNone s = withGameScene s model |> sansCmd
                kbTitleCase spot =
                        case spot of
                                "1" -> sceneNone (GameConfig defaultGameConfigModel ) 
                                "2" -> sceneNone (Tutorial TeachingMoving)
                                "3" -> sceneNone Options
                                "4" -> sceneNone Credits
                                _ -> (model, Cmd.none)
        
                mouseTitleCase menu spot =
                        kbTitleCase 
                                (
                                        (model.width |> toFloat)
                                        / (menu |> optionsForMenu |> List.length |> toFloat)
                                        |> floor 
                                        |> (//) spot 
                                        |> (+) 1
                                        |> String.fromInt)
        in
        case msg of
        KeyPress spot ->
                case model.gameScene of
                        Battle bModel ->
                                let
                                        bMsg = 
                                                case spot of
                                                        "1" -> SelectPiece 1
                                                        "2" -> SelectPiece 2
                                                        "3" -> SelectPiece 3
                                                        "4" -> SelectPiece 4 
                                                        "5" -> SelectPiece 5
                                                        _ -> DeselectPiece
                                        (gScene, cmds) = updateBattleScene bMsg bModel
                                in
                                ( { model | gameScene = gScene }
                                , Cmd.batch [cmds, Cmd.none]
                                )
                        Title ->
                                kbTitleCase spot
                        GameConfig gcModel ->
                                case gcModel of
                                        NothingConfigured ->
                                                case spot of
                                                        "1" -> sceneNone (GameConfig AIGameConfigured)
                                                        "2" -> sceneNone (GameConfig (SDVChosen ""))
                                                        "3" -> sceneNone (Title)
                                                        _ -> (model, Cmd.none)
                                        SDVChosen n ->
                                                case spot of
                                                        "1" -> sceneNone (GameConfig (SDVChosenOneName n ""))
                                                        "2" -> sceneNone (GameConfig NothingConfigured)
                                                        c -> sceneNone (GameConfig (SDVChosen (n++c)))
                                        SDVChosenOneName n1 n2 ->
                                                case spot of
                                                        "1" -> sceneNone (GameConfig (SDVConfigured n1 n2))
                                                        "2" -> sceneNone (GameConfig (SDVChosen n1))
                                                        c -> sceneNone (GameConfig (SDVChosenOneName n1 (n2++c)))
                                        AIGameConfigured ->
                                                case spot of
                                                        "1" -> sceneNone (Battle defaultBattleModel)
                                                        _ -> sceneNone (GameConfig NothingConfigured)
                                        SDVConfigured n1 n2 ->
                                                case spot of
                                                        "1" -> sceneNone (Battle defaultBattleModel)
                                                        _ -> sceneNone (GameConfig (SDVChosenOneName n1 n2))
                        _ -> ( model, Cmd.none )
        Click spot ->
                case model.gameScene of
                        Title ->
                                mouseTitleCase titleMenu spot
                        _ -> ( model, Cmd.none )
        SetWindowSize w h ->
                { model
                        | width = w
                        , height = h
                } |> sansCmd
        _ ->
                (model, Cmd.none)

type alias PlayerModel =
        { name : String
        , army : Army
        , health : Int 
        }

defaultBattleModel : BattleModel
defaultBattleModel =
        { player1 =
                { name = "player1"
                , army = Army 
                        (Active Orange Authentic)
                        (Active Blue Authentic)
                        (Active Orange Deceitful)
                        (Obliterated)
                        (Active Blue Authentic)
                , health = 5
                }
        , player2 =
                { name = "player2"
                , army = Army 
                        (Active Orange Authentic)
                        (Active Orange Authentic)
                        (Active Orange Authentic)
                        (Active Blue Authentic)
                        (Active Blue Authentic)
                , health = 5
                }
        , battlePhase = PlayerTurn (Turn First Nothing)
        , playMode = SingleDeviceVs 
        }


init : () -> ( Model, Cmd Msg)
init _ =
        (       { gameScene = Title --Battle defaultBattleModel
                , config = initConfig
                , debugMode = True
                , width = 0
                , height = 0
                , helpOverlay = True
                }
                
        , Browser.Dom.getViewport |> Task.attempt (\r ->
                case r of
                        Ok vp ->
                                SetWindowSize (floor vp.viewport.width) (floor vp.viewport.height)
                        Err e ->
                                NoOp
                )
        )

main = Browser.element 
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
