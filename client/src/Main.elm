module Main exposing (main)

import Svg exposing (Svg)
import Svg.Attributes
import Svg.Events
import Html exposing (Html)
import Cylinder3d
import Html.Attributes
import Basics.Extra exposing (flip) 
import Browser.Dom
import Html.Events
import Time
import Array exposing (Array)
import Browser
import Browser.Events
import Json.Decode as Decode
import Point2d
import Rectangle3d
import SketchPlane3d
import Graphics exposing (..)
import Task
import Rectangle2d
import Vector3d
import Browser.Dom
import Angle
import Scene3d.Light
import Luminance
import Illuminance
import Logic exposing (PlayerModel, BattleAction(..), Spot, Army(..), Honesty(..), Color(..), Piece(..), updateHonestyAt, updateColorAt, initialArmy, armyFromList, armyPiecesList)
import LuminousFlux
import Axis3d
import Scene3d
import Point3d
import Point3d.Projection
import Color as StdColor
import Block3d
import Scene3d.Material as Material
import Camera3d
import Viewpoint3d
import Direction3d
import Angle
import Length
import Pixels


        
samePieceColor piece1 piece2 =
        case (piece1, piece2) of
                (Piece Orange _, Piece Orange _) -> True
                (Piece Blue _, Piece Blue _) -> True
                (_, _) -> False


{-
type alias Model =
    { gameScene : GameScene
    , player1Name : Name
    , player2Name : Name
    , config : Config




type GameScene
    = Title
    | GameConfig GameConfigModel
    | Options
    | Credits
    | Tutorial TutorialScene
    | Transition
    | Placement PlacementScene
    | Battle Logic.BattleModel

type alias Model =
    { gameScene : GameScene
    , config : Config
    , debugMode : Bool
    , width : Int
    , transitions : List (Animation, Delay, CompleteIn)
    , height : Int
    , helpOverlay : Bool
    }
-}

type alias Duration = Float

{-type alias Model =
    { screen : ScreenDimensions
    , config : Config
    , lastKey : String
    , scene : Phase
    }
-}
        
type Phase
    = Scene Scene
    | Transition Phase Phase
--  | ControlPass Phase

type alias Core =
    { graphics : Graphics.Engine
    , config : Config
    , scene : Scene
    }

type alias CoreSystem =
    { width : Int
    , height : Int
    , config : Config
    , lastKey : String
    }

type Scene 
    = Title 
    | GameConfig GameConfigModel 
    | Battle PieceSelected Logic.BattleState 
    | Cutscene 
    --| SomethingForOtherGames


battleStateForPhase phase =
    case phase of
        (Scene (Battle _ bs)) -> bs
        _ -> Logic.initializeBattle 


type alias PieceSelected = Maybe Int

type PhaseShift
    = TitlePS TitleMsg
    | GameConfigPS GameConfigMsg
    | BattlePS BattleMsg
    | KeyFrameShift Float
    | SetWindowSize Int Int
    | NoOp



type TitleMsg
    = SelectNewGame
    | SelectTutorial
    | SelectOptions
    | SelectCredits
{-
type GameScene 
    = Title
    | GameConfig GameConfigModel
    | Options
    | Credits
    | Battle BattleModel
    | Tutorial TutorialPhase
    | Transition Model Model
-}

type GameConfigModel
    = NothingConfigured
    | SDVChosen GameModel
    | SDVChosenOneName GameModel
    | SDVP1ChoosingPieces Spot GameModel
    | SDVP2ChoosingPieces Spot GameModel
    | AIGameConfigured GameModel
    | SDVConfigured GameModel


gcmGameModel gcm =
    case gcm of
        SDVChosen m -> m
        SDVChosenOneName m -> m
        SDVP1ChoosingPieces _ m -> m
        SDVP2ChoosingPieces _ m -> m
        AIGameConfigured m -> m
        SDVConfigured m -> m
        NothingConfigured -> initGameModel


type alias GameModel =
    { p1 : Name
    , p2 : Name
    , p1a : PlacementModel
    , p2a : PlacementModel
    }


initGameModel =
    { p1 = ""
    , p2 = ""
    , p1a = Array.fromList []
    , p2a = Array.fromList []
    }

type GameConfigMsg 
    = SetPlayModeAI
    | SetPlayModeSingleDevice
    | SetPlayer1Name String
    | SetPlayer2Name String
    | SetPlayer1Piece Piece
    | SetPlayer2Piece Piece 
    | ConfirmConfig 
    | GoBack

defaultGameConfigModel : GameConfigModel
defaultGameConfigModel =
    NothingConfigured

type alias Name = String

type TurnNumber
    = First
    | Second

type BattleMsg
    = SelectPiece Int
    | MovePiece Int Int
    | FirePiece Int
    | DeselectPiece
    | WatchRecap
    | SkipRecap
    | Terminate

type PlayMode
    = VsCPU
    | SingleDeviceVs
    

initialPlayerPieces =
    [ Piece Orange Authentic
    , Piece Orange Authentic
    , Piece Orange Authentic
    , Piece Blue Authentic
    , Piece Blue Authentic
    ]
    
initialOpponentPieces =
    [ Piece Orange Authentic
    , Piece Orange Authentic
    , Piece Blue Authentic
    , Piece Blue Authentic
    , Piece Blue Authentic
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


type alias Delay = Float
type alias CompleteIn = Float

{-
incrTurnNumber turnPhase =
        case turnPhase of
                Turn First _ -> Evaluation First
                Turn Second _ -> Evaluation Second
                other -> other
-}


type GameActions 
        = FireOn Int
        | MoveTo Int


        {-
type Msg
        = NoOp
        | SetWindowSize Int Int 
        | Click Int 
        | KeyPress String
        | BattleSceneMsg BattleMsg
        | TitleMenuSceneMsg TitleMenuMsg
        | GameConfigSceneMsg GameConfigMsg
        | KeyFrame Float
        
type Menu
        = Menu (List MenuOption)
        | Confirmation
        | ArmyList
        -}

--type alias MenuOption = String

{-
optionsForMenu menu =
        case menu of
                Menu opts -> opts
                Confirmation -> ["Confirm", "Cancel"]
                ArmyList -> ["1", "2", "3", "4", "5"]
-}
--battleMenu = ArmyList

type PlacementAction
        = SetColor Spot Color
        | SetHonesty Spot Honesty


-- gameplayscreen views


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




menuArmy count = 
        case count of 
                1 -> Army (Piece Orange Authentic) Obliterated Obliterated Obliterated Obliterated
                2 -> Army (Piece Orange Authentic) (Piece Orange Authentic) Obliterated Obliterated Obliterated
                3 -> Army (Piece Orange Authentic) (Piece Orange Authentic) (Piece Orange Authentic) Obliterated Obliterated
                4 -> Army (Piece Orange Authentic) (Piece Orange Authentic) (Piece Orange Authentic) (Piece Orange Authentic) Obliterated
                5 -> Army (Piece Orange Authentic) (Piece Orange Authentic) (Piece Orange Authentic) (Piece Orange Authentic) (Piece Orange Authentic) 
                _ -> Army Obliterated Obliterated Obliterated Obliterated Obliterated


type alias Prompt = (Phase -> List (Html PhaseShift))
type alias ClickDecoder = (Phase -> Decode.Decoder PhaseShift)
type alias MouseDecoder = (Phase -> Decode.Decoder PhaseShift)

type alias MenuOpts =
    { click : ClickDecoder 
    --, mouse : MouseDecoder model msg
    }

type Menu 
    = Menu MenuOpts 
    | Confirmation MenuOpts 

titleMenu = Menu { click = titleMenuDecoder } --Menu ["Start Game", i"Tutorial", "Options", "Credits"]nn


titleOpts = ["Start Game", "Tutorial", "Options", "Credits"]

titleMenuText width height = 
    List.indexedMap (Graphics.scene3dText width height ["menuSize", "blackFill"]) titleOpts
    |> Svg.svg 
        [ Svg.Attributes.viewBox ("0 0 " ++ (String.fromInt width) ++ " " ++ (String.fromInt height))
        , Svg.Attributes.width (String.fromInt width)
        , Svg.Attributes.height (String.fromInt height)
        ]

titleMenuEntities =
    let
        totalOpts = titleOpts |> List.length
        pieces = 
            List.indexedMap (entityView Near) (totalOpts |> menuArmy |> armyPiecesList)
        bars =
            (healthView totalOpts Far)
    in
    pieces ++ bars 

titleMenuView width height phase =
    menuView width height (titleMenuText width height) (titleMenuEntities)

gcOpts gcm =
            case gcm of
                NothingConfigured ->
                    ["AI Battle", "2P Battle", "Go back"]
                SDVChosen gm ->
                    []
                SDVChosenOneName gm ->
                    []
                SDVP1ChoosingPieces _ gm ->
                    []
                SDVP2ChoosingPieces _ gm ->
                    []
                SDVConfigured gm ->
                    ["Start!", "Go back"]
                AIGameConfigured gm ->
                    ["Start!", "Go back"]

gcMenuEntities gcm =
    let 
        totalOpts = gcOpts gcm  |> List.length
        pieces = List.indexedMap (entityView Near) (totalOpts |> menuArmy |> armyPiecesList)
        bars = (healthView totalOpts Far)
    in
    pieces ++ bars


gcMenuText width height gcm =
    List.indexedMap (Graphics.scene3dText width height ["menuSize", "purpleFill"]) (gcOpts gcm)
    |> Svg.svg
        [ Svg.Attributes.viewBox ("0 0 " ++ (String.fromInt width) ++ " " ++ (String.fromInt height))
        , Svg.Attributes.width (String.fromInt width)
        , Svg.Attributes.height (String.fromInt height)
        ]

textEntryView gcm =
    let
        confirmBttn = Html.button 
                        [ Html.Events.onClick (GameConfigPS ConfirmConfig)
                        ] 
                        [ Html.text "Confirm" ]
        cancelBttn = Html.button
                        [ Html.Events.onClick (GameConfigPS GoBack)
                        ] 
                        [ Html.text "Go back" ]
    in
    case gcm of
        SDVChosen model ->
            [ Html.section [ Html.Attributes.class "full-flex" ]
                [ Html.h1 [] [ Html.text "Enter your name" ]
                , Html.input 
                    [ Html.Attributes.type_ "text"
                    , Html.Attributes.id "player-1-name-entry"
                    , Html.Events.onInput (GameConfigPS << SetPlayer1Name)
                    , Html.Attributes.value model.p1
                    ] []
                , Html.menu 
                    []
                    [ confirmBttn 
                    , cancelBttn 
                    ]
                ]
            ]
        SDVChosenOneName model ->
            [ Html.section [ Html.Attributes.class "full-flex" ]
                [ Html.h1 [] [ Html.text "Enter your opponent's name" ]
                , Html.input 
                    [ Html.Attributes.type_ "text"
                    , Html.Attributes.id "player-2-name-entry"
                    , Html.Events.onInput (GameConfigPS << SetPlayer2Name)
                    , Html.Attributes.value model.p2
                    ] []
                , Html.menu 
                    []
                    [ confirmBttn 
                    , cancelBttn 
                    ]
                ]
            ]
        _ ->
            [ Html.text "" ]


gcMenuView width height phase gcm =
    case gcm of
        NothingConfigured -> 
            menuView width height (gcMenuText width height gcm) (gcMenuEntities gcm)
        SDVChosen _ ->
            textEntryView gcm
        SDVChosenOneName _ ->
            textEntryView gcm
        SDVP1ChoosingPieces _ _ ->
            placementView width height phase gcm
        SDVP2ChoosingPieces _ _ ->
            placementView width height phase gcm
        AIGameConfigured _ ->
            menuView width height (gcMenuText width height gcm) (gcMenuEntities gcm)
        SDVConfigured _ ->
            menuView width height (gcMenuText width height gcm) (gcMenuEntities gcm)


menuView : Int -> Int -> (Html PhaseShift) -> List (Scene3d.Entity coordinates) -> List (Html PhaseShift)
menuView width height textOverlay entities =
    let
        mainLight =
            Scene3d.Light.point (Scene3d.Light.castsShadows True)
                { chromaticity = Scene3d.Light.incandescent
                , intensity = LuminousFlux.lumens 10000
                , position = Point3d.meters 0 0 4
                }
        cust e =
            Scene3d.custom
                { entities = e
                , camera = camera
                , clipDepth = Length.meters 1
                , background = Scene3d.backgroundColor StdColor.black
                , dimensions = ( Pixels.pixels width, Pixels.pixels height )
                , lights = Scene3d.oneLight mainLight
                , exposure = Scene3d.exposureValue 8 
                , toneMapping = Scene3d.noToneMapping
                , whiteBalance = Scene3d.Light.daylight
                , antialiasing = Scene3d.multisampling
                }

    in
    [ textOverlay
    , cust entities
    ]

    --Html.main_ [] [ Html.text "Success" ]
    {-
menuView =
        menuViewPlusEntities []

menuWithNormalPieces width height menu = 
    menuViewPlusEntities (menuViewPieces menu) width height menu


menuViewPieces menu =
    menu
    |> optionsForMenu
    |> List.length
    |> menuArmy
    |> armyNearView



menuViewPlusEntities entities width height menu =
        let 
                options = optionsForMenu menu
                totalOptions = List.length options
                hv = (healthView totalOptions Near)
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
                                (List.indexedMap (\i o -> scene3dText o i width height) options)
        in
        [ text
        , Scene3d.custom
                { entities = entities ++ hv 
                , camera = camera
                , clipDepth = Length.meters 1
                , background = Scene3d.backgroundColor StdColor.black
                , dimensions = ( Pixels.pixels width, Pixels.pixels height )
                , lights = Scene3d.oneLight mainLight
                , exposure = Scene3d.exposureValue 8 
                , toneMapping = Scene3d.noToneMapping
                , whiteBalance = Scene3d.Light.daylight
                , antialiasing = Scene3d.multisampling
                }
        ]
    -}

{-
battleView : Model -> BattleModel -> Html Msg
battleView {width, height, gameScene, transitions} {player1, player2, battleState} =
    let
        defaultP1 = armyNearView (Logic.player1Army battleState)
        defaultP2 = armyFarView (Logic.player2Army battleState)
        (pent, oppent) =
            case gameScene of 
                Transition oldModel _ ->
                    case oldModel.gameScene of
                        Battle bMod ->
                            case bMod.battlePhase of
                                PlayerTurn _ ->
                                    case transitions of 
                                        (PlayerFire spot, _, tRem)::_ ->
                                            ( shotAnimation True Orange spot (tRem/1000)
                                            , armyFarView (Logic.player2Army battleState)
                                            )
                                        (PlayerMove spot1 spot2, _, tRem)::_ ->
                                            ( moveAnimation True (Logic.player1Army battleState) spot1 spot2 (tRem/600)
                                            , armyFarView (Logic.player2Army battleState)
                                            )
                                        _ -> 
                                            (armyNearView (Logic.player1Army battleState), armyFarView (Logic.player2Army battleState))
                                OpponentTurn ->
                                    case transitions of 
                                        (OpponentFire spot, _, tRem)::_ ->
                                            ( armyNearView (Logic.player1Army battleState)
                                            , shotAnimation False Orange spot (tRem/1000)
                                            )
                                        (PlayerMove spot1 spot2, _, tRem)::_ ->
                                            ( armyNearView (Logic.player1Army battleState)
                                            , moveAnimation False (Logic.player2Army battleState) spot1 spot2 (tRem/600)
                                            )
                                        _ -> 
                                            (armyNearView (Logic.player1Army battleState), armyFarView (Logic.player2Army battleState))
                                _ ->
                                    (armyNearView (Logic.player1Army battleState), armyFarView (Logic.player2Army battleState))
                        _ ->
                            (defaultP1, defaultP2)
                _ ->
                    (defaultP1, defaultP2)
        hv = (healthView (Logic.player2Health battleState) Far)
                ++ (healthView (Logic.player1Health battleState) Near)
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
                { entities = hv ++ pent ++ oppent
                , camera = camera
                , clipDepth = Length.meters 1
                , background = Scene3d.backgroundColor StdColor.black
                , dimensions = ( Pixels.pixels width, Pixels.pixels height )
                , lights = Scene3d.oneLight mainLight
                , exposure = Scene3d.exposureValue 8 
                , toneMapping = Scene3d.noToneMapping
                , whiteBalance = Scene3d.Light.daylight
                , antialiasing = Scene3d.multisampling
                }
-}




healthView total facing =
        let
                healthy = 
                        Scene3d.quad (Material.emissive Scene3d.Light.daylight (Luminance.nits 300))
                dead =
                        Scene3d.quad (Material.color StdColor.red)
                mapFn index entities =
                        spotTranslation index entities |> faceHealthIndicators facing
        in
        List.repeat total (healthBlock healthy)
        ++ List.repeat (5-total) (healthBlock dead)
        |> List.indexedMap (mapFn)
                

faceHealthIndicators : Distance -> Scene3d.Entity coordinates -> Scene3d.Entity coordinates
faceHealthIndicators facing =
        case facing of
               Far -> Scene3d.translateBy (Vector3d.meters -11 0 0)
               Near -> Scene3d.rotateAround Axis3d.y (Angle.degrees 90)
                        >> Scene3d.translateBy (Vector3d.meters 0.8 0 -2)

        {-
passControlView : BattleModel -> Svg Msg
passControlView bmodel =
        Html.article [ Html.Attributes.id "pass-control-view" ]
                [ Html.p [] [ Html.text "Please pass control of device to next player" ]
                --, Html.button [ Html.Events.onClick (BattleSceneMsg WatchRecap) ] [ Html.text "Watch recap" ]
                , Html.button [ Html.Events.onClick (KeyPress "1") ] [ Html.text "Skip >" ]
                ]
        -}


viewBoxFor width height =
        Svg.Attributes.viewBox ("0 0 " ++ (String.fromInt width) ++ " " ++ (String.fromInt height))

fullScreenSvgAttrs width height =
        [ Svg.Attributes.viewBox ("0 0 " ++ (String.fromInt width) ++ " " ++ (String.fromInt height))
        , Svg.Attributes.width (String.fromInt width)
        , Svg.Attributes.height (String.fromInt height)
        ]

type alias PlacementModel =
        Array Piece

type PlacementOptions 
    = Any
    | Only Color

threePlacements : PlacementModel -> PlacementOptions
threePlacements pmodel =
        pmodel
        |> Array.toList
        |> List.filterMap (\p ->
            case p of
                Piece Orange _ ->
                    Just True
                Piece Blue _ ->
                    Just False
                _ ->
                    Nothing
            ) 
        |> List.partition identity
        |> Tuple.mapBoth List.length List.length
        |> (\(orange, blue) -> if orange == 3 then Only Blue else if blue == 3 then Only Orange else Any)

        
boxMenuItemTranslations : Spot -> Piece -> Scene3d.Entity coordinates
boxMenuItemTranslations spot piece =
        pieceView piece
        |> Scene3d.scaleAbout Point3d.origin 0.6 
        |> p2faceoff Near
        |> spotTranslation spot
        |> Scene3d.translateBy (Vector3d.meters -10 0 0)
        

placementOptions =
    [ Piece Blue Authentic, Piece Blue Deceitful
    , Piece Orange Authentic, Piece Orange Deceitful ]
    {-    case threePlacements model of
                Only Blue ->
                        [ Piece Blue Authentic, Piece Blue Deceitful ]
                Only Orange ->
                        [ Piece Orange Authentic, Piece Orange Deceitful ]
                Any ->
                        [ Piece Blue Authentic, Piece Blue Deceitful ]
                        ++ [ Piece Orange Authentic, Piece Orange Deceitful ]

pOpts = 

    -}
placementView : Int -> Int -> Phase -> GameConfigModel -> List (Html PhaseShift)
placementView width height phase gcm =
    let
        currentArmy =
            case gcm of 
                SDVP1ChoosingPieces _ gm ->
                    gm.p1a
                SDVP2ChoosingPieces _ gm ->
                    gm.p2a
                _ -> Logic.initialArmy |> Logic.armyPiecesList |> Array.fromList
        currentBox = Array.length currentArmy
        armyEntities = currentArmy |> Array.toList |> Logic.armyFromList |> armyNearView 
        highlightEntity = 
            if currentBox == 5 then
                []
            else 
                [ spotHighlighter currentBox ]
        boxEntityMapper = 
                List.indexedMap boxMenuItemTranslations 
        (textOptions, optionEntities) =
            if currentBox == 5 then
                (["Confirm", "Undo"], [])
            else
                case threePlacements currentArmy of
                    Only Orange ->
                        (["Honest Orange", "Deceitful Orange"], boxEntityMapper (List.drop 2 placementOptions))
                    Only Blue ->
                        (["Honest Blue", "Deceitful Blue"], boxEntityMapper (List.take 2 placementOptions))
                    Any ->
                        (["Honest Blue", "Deceitful Blue", "Honest Orange", "Deceitful Orange"], boxEntityMapper placementOptions)
        entities = highlightEntity ++ armyEntities ++ optionEntities ++ (healthView (List.length textOptions) Far)
        text = 
            List.indexedMap (Graphics.scene3dText width height ["menuSize", "purpleFill"]) textOptions
            |> Svg.svg 
                [ Svg.Attributes.viewBox ("0 0 " ++ (String.fromInt width) ++ " " ++ (String.fromInt height))
                , Svg.Attributes.width (String.fromInt width)
                , Svg.Attributes.height (String.fromInt height)
                ]
    in
    menuView width height text entities 
            
battleView : Int -> Int -> Phase -> PieceSelected -> Logic.BattleState -> List (Html PhaseShift)
battleView width height phase selected bState =
    let
        playerDrawer = 
            case phase of
                Transition _ p1 p2 ->
                    case p1 of 
                        Scene (Cutscene ((Graphics.PlayerFire s, delay, dur)::_)) ->
                            Graphics.armyNearView >> ((++) (Graphics.shotAnimation True Logic.Orange s (dur/1000)))
                        _ -> Graphics.armyNearView
                Scene bscene ->
                    Graphics.armyNearView
        names = []
        selectedEntity = selected |> Maybe.map ((+) -1 >> battleSpotHighlighter >> List.singleton) |> Maybe.withDefault []
        playerHealth = healthView (Logic.player1Health bState) Near
        opponentHealth = healthView (Logic.player2Health bState) Far
        (playerArmy, opponentArmy) = 
            (bState, bState)
            |> Tuple.mapBoth 
                (Logic.player1Army >> Graphics.armyNearView) 
                (Logic.player2Army >> Graphics.armyFarView)
        entities =
            playerHealth 
            ++ opponentHealth 
            ++ playerArmy 
            ++ opponentArmy 
            ++ names 
            ++ selectedEntity 
        text = "Opponent"
            |> String.toUpper
            |> Graphics.scene3dText width height [ "signSize", "purpleFill" ] -2
            |> List.singleton
            |> Svg.svg 
                [ Svg.Attributes.viewBox ("0 0 " ++ (String.fromInt width) ++ " " ++ (String.fromInt height))
                , Svg.Attributes.width (String.fromInt width)
                , Svg.Attributes.height (String.fromInt height)
                ]
    in
    bView width height text entities
    --menuView width height phase text entities


bView : Int -> Int -> Html PhaseShift -> List (Scene3d.Entity coordinates) -> List (Html PhaseShift)
bView w h text es =
    menuView w h text es


{-
gameConfigView : GameConfigModel -> Int -> Int -> List (Html Msg)
gameConfigView gcModel width height=
        let 
                cancelButton = 
                        [ Html.button [ Html.Events.onClick (KeyPress "2") ] [ Html.text "Cancel" ] ]
                confirmButton = 
                        [ Html.button [ Html.Events.onClick (KeyPress "1") ] [ Html.text "Confirm" ] ]
                nameInput msg val = 
                        [ Html.input [ Html.Events.onInput msg ] [ ]
                        ]
        in
        case gcModel of 
                NothingConfigured ->
                        menuWithNormalPieces width height (Menu ["AI Battle", "2P", "Cancel"])
                AIGameConfigured ->
                        menuWithNormalPieces width height (Menu ["Confirm - AI Game", "Cancel"])
                SDVChosen name ->
                        [ Html.section [ Html.Attributes.class "modal" ]
                                (Html.h1 [] [ Html.text "Enter Your Name" ]
                                :: nameInput (GameConfigSceneMsg << SetPlayer1Name) name
                                ++ confirmButton
                                ++ cancelButton
                        )--, menuView...
                        ]
                SDVChosenOneName _ name ->
                        [ Html.section [ Html.Attributes.class "modal" ]
                                (Html.h1 [] [ Html.text "Enter Your Opponent's Name" ]
                                :: nameInput (GameConfigSceneMsg << SetPlayer1Name) name
                                ++ confirmButton
                                ++ cancelButton
                        )--, menuView...
                        ]
                SDVConfigured _ _ ->
                        [ Html.h1 [ Html.Attributes.class "modal" ] [ Html.text "Starting your match..." ] 
                        ]
                        ++ confirmButton
                        ++ cancelButton
                SDVP1ChoosingPieces (_, p) _ ->
                    placementView width height p
                SDVP2ChoosingPieces _ (_, p) ->
                    placementView width height p
-}
{-
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
                 , Browser.Events.onAnimationFrameDelta (KeyFrame)
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
                                                        [ battleView model gpm ]
                                                        {-[ Svg.svg 
                                                                [ Svg.Attributes.viewBox "0 0 100 100" ]
                                                                [ boardView gpm ]
                                                                ]-}
                                Title ->
                                        menuWithNormalPieces model.width model.height titleMenu
                                GameConfig gcm ->
                                        gameConfigView gcm model.width model.height
                                Transition current _ ->
                                    case (current.gameScene, model.transitions) of
                                        (Title, (PlayerFire spot, _, tRem)::_) ->
                                            menuViewPlusEntities
                                                ((menuViewPieces titleMenu)
                                                ++ shotAnimation True Orange spot (tRem/1000))
                                                model.width model.height titleMenu
                                        (Battle bMod, _) ->
                                            [ battleView model bMod ]
                                        _ ->
                                            [ Html.text "" ]

                                _ -> [ Html.text "" ]
                els = 
                        if model.helpOverlay then
                                helpOverlayView model :: fn
                        else
                                fn
        in
        Html.main_ [] els
                                                        -}

titleMenuDecoder : Phase -> Decode.Decoder PhaseShift
titleMenuDecoder _ =
    Decode.map 
        (\s ->
            case s of
                "1" -> TitlePS SelectNewGame
                "2" -> TitlePS SelectTutorial
                "3" -> TitlePS SelectOptions
                "4" -> TitlePS SelectCredits
                _ -> NoOp
        )
        (Decode.field "key" Decode.string)
                
gcMenuDecoder : Phase -> Decode.Decoder PhaseShift
gcMenuDecoder phase =
    let
        confirmDeny s =
            case s of
                "1" -> GameConfigPS ConfirmConfig
                "2" -> GameConfigPS GoBack 
                "Enter" -> GameConfigPS ConfirmConfig
                "Escape" -> GameConfigPS GoBack
                _ -> NoOp
        textEntryMenu gcm s event =
            case s of
                "Enter" -> GameConfigPS ConfirmConfig
                "Escape" -> GameConfigPS GoBack
                x -> event (gcm.p1 ++ x) |> GameConfigPS
        twoColors evt s hPiece dPiece spot =
            case s of
                "1" -> GameConfigPS (evt hPiece spot)
                "2" -> GameConfigPS (evt dPiece spot)
                "3" -> GameConfigPS (GoBack)
                _ -> NoOp
        optsForPlacement placement =
            case placement of
                Only Blue ->
                    List.take 2 placementOptions |> Array.fromList
                Only Orange ->
                    List.drop 2 placementOptions |> Array.fromList
                Any ->
                    placementOptions |> Array.fromList
        forPlacement evt spot key placement =
            let
                opts = optsForPlacement placement
                numOpts = Array.length opts
            in
            key
            |> String.toInt
            |> Maybe.andThen ((+) -1 >> flip Array.get opts)
            |> Maybe.map (evt >> GameConfigPS)
            |> (\mev ->
                case mev of
                    Just ev -> ev
                    Nothing -> if key == (numOpts + 1 |> String.fromInt)  then GameConfigPS GoBack else NoOp
                )
    in
    Decode.map 
        (\s ->
            case phase of
                Scene (GameConfig (NothingConfigured) _) ->
                    case s of
                        "1" -> GameConfigPS SetPlayModeAI
                        "2" -> GameConfigPS SetPlayModeSingleDevice
                        "3" -> GameConfigPS GoBack
                        _ -> NoOp
                Scene (GameConfig (SDVChosen gcm) _) ->
                    NoOp
                Scene (GameConfig (SDVChosenOneName gcm) _) ->
                    NoOp
                Scene (GameConfig (SDVP1ChoosingPieces newSpot gcm) _) ->
                    forPlacement SetPlayer1Piece newSpot s (threePlacements gcm.p1a)
                Scene (GameConfig (SDVP2ChoosingPieces newSpot gcm) _) ->
                    forPlacement SetPlayer2Piece newSpot s (threePlacements gcm.p2a)
                Scene (GameConfig (SDVConfigured gcm) _) ->
                    confirmDeny s        
                Scene (GameConfig (AIGameConfigured gcm) _) ->
                    confirmDeny s
                _ -> NoOp
        )
        (Decode.field "key" Decode.string)
        
numAcceptable n =
    if n <= 5 && n > 0 then True else False

battleMenuDecoder : Phase -> Decode.Decoder PhaseShift
battleMenuDecoder phase =
    Decode.map
        (String.toInt 
            >> Maybe.andThen (\s -> if numAcceptable s then Just <| BattlePS <| SelectPiece s else Nothing)
            >> Maybe.withDefault NoOp)
        (Decode.field "key" Decode.string)


battleMenuSelectedDecoder : Phase -> Int -> Decode.Decoder PhaseShift 
battleMenuSelectedDecoder phase piece =
    Decode.map
        (String.toInt
            >> Maybe.map (\s -> 
                if numAcceptable s then
                    if s == piece then
                        BattlePS <| FirePiece piece
                    else
                        BattlePS <| MovePiece s piece
                else
                    BattlePS DeselectPiece
                )
            >> Maybe.withDefault (BattlePS DeselectPiece)
        )
        (Decode.field "key" Decode.string)



mapPhaseCore fn phase =
    case phase of
        Scene (Title core) -> fn core |> Title |> Scene
        Scene (GameConfig gcm core) -> fn core |> GameConfig gcm |> Scene
        Scene (Battle t bm core) -> fn core |> Battle t bm |> Scene
        Scene (Cutscene anis core) -> fn core |> Cutscene anis |> Scene
        Transition duration p1 p2 -> Transition duration (mapPhaseCore fn p1) p2 

update : PhaseShift -> Model -> (Phase, Cmd PhaseShift)
update ps model =
    let 
        phase = model.phase
        sansCmd x = (x, Cmd.none)
        transitionWithAni : Float -> List (Animation, Delay, Duration) -> Phase -> Phase
        transitionWithAni dur anis next =
            Transition (Scene (Cutscene anis )) next
        unwrapTransitonIfEnded t =
            case t of
                Transition (Scene (Cutscene (((_, _, d) as h)::r) )) p2 ->
                    if d < 0 then
                        if List.length r > 0 then
                            Transition (Scene (Cutscene r )) p2
                        else
                            p2
                    else
                        p2
                Transition (Scene _) p2 ->
                    if d < 0 then
                        p2
                    else
                        t
                _ -> t
    in
    case ps of
        SetWindowSize w h ->
            model
            |> mapGraphics (Graphics.setWindow w h)
            |> sansCmd
        KeyFrameShift delta ->
            model
            |> mapGraphics (Graphics.nextFrame delta)
            |> sansCmd 

            case phase of 
                Transition duration (Scene (Cutscene ((ani, delay, duration2)::remaining) core1)) p2 ->
                    Transition (duration-delta) (Scene (Cutscene ((ani,delay-delta,duration2-delta)::remaining) core1)) p2
                    |> unwrapTransitonIfEnded
                    |> sansCmd
                Transition duration p1 p2 ->
                    Transition (duration-delta) p1 p2
                    |> unwrapTransitonIfEnded
                    |> sansCmd
                p ->
                    p |> sansCmd
        GameConfigPS gcm ->
            case (gcm, phase) of
                (SetPlayModeAI, Scene (GameConfig NothingConfigured _)) ->
                    AIGameConfigured 
                        { p1 = "Human"
                        , p2 = "Computer"
                        , p1a = [] |> Array.fromList
                        , p2a = 
                            [ Logic.honestBlue, Logic.honestBlue, Logic.dishonestBlue, Logic.honestOrange, Logic.honestOrange ] 
                            |> Array.fromList
                        }
                    |> flip GameConfig core >> Scene >> transitionWithAni 1000 [(PlayerFire 1, 0, 1000)]
                    |> sansCmd
                (SetPlayModeSingleDevice, Scene (GameConfig NothingConfigured _)) ->
                    GameConfig (SDVChosen initGameModel) core
                    |> Scene >> transitionWithAni 1000 [(PlayerFire 1, 0, 1000)]
                    |> sansCmd
                (SetPlayer1Name str, Scene (GameConfig (SDVChosen gcModel) _)) ->
                    { gcModel | p1 = str }  
                    |> SDVChosen >> flip GameConfig core >> Scene
                    |> sansCmd
                (SetPlayer2Name str, Scene (GameConfig (SDVChosenOneName gcModel) _)) ->
                    { gcModel | p2 = str }
                    |> SDVChosenOneName >> flip GameConfig core >> Scene
                    |> sansCmd
                (SetPlayer1Piece piece, Scene (GameConfig (SDVP1ChoosingPieces spot gcModel) _)) -> 
                    { gcModel | p1a = gcModel.p1a |> Array.push piece }
                    |> SDVP1ChoosingPieces (spot + 1) >> flip GameConfig core >> Scene
                    |> sansCmd
                (SetPlayer2Piece piece, Scene (GameConfig (SDVP2ChoosingPieces spot gcModel) _)) ->
                    { gcModel | p2a = gcModel.p2a |> Array.push piece }
                    |> SDVP2ChoosingPieces (spot + 1) >> flip GameConfig core >> Scene
                    |> sansCmd
                (ConfirmConfig, Scene (GameConfig confMod _)) -> 
                    let
                        focusFn idStr =
                            Task.attempt (\res ->
                                case res of
                                    Err (Browser.Dom.NotFound reason) ->
                                        Debug.log reason NoOp
                                    _ ->
                                        NoOp
                            )
                            (Browser.Dom.focus idStr)
                    in

                    case confMod of
                        SDVChosen gcModel ->
                            SDVChosenOneName gcModel 
                            |> flip GameConfig core >> Scene
                            |> sansCmd
                        SDVChosenOneName gcModel ->
                            SDVP1ChoosingPieces 1 gcModel 
                            |> flip GameConfig core >> Scene
                            |> flip Tuple.pair (focusFn "player-1-name-entry")
                        SDVP1ChoosingPieces _ gcModel ->
                            SDVP2ChoosingPieces 1 gcModel 
                            |> flip GameConfig core >> Scene
                            |> flip Tuple.pair (focusFn "player-2-name-entry")
                        SDVP2ChoosingPieces _ gcModel ->
                            SDVConfigured gcModel 
                            |> flip GameConfig core >> Scene
                            |> sansCmd
                        AIGameConfigured gcModel ->
                            Logic.startBattle 
                                (gcModel.p1a |> Array.toList |> armyFromList) 
                                (gcModel.p2a |> Array.toList |> armyFromList)
                            |> flip (Battle Nothing) core >> Scene
                            |> sansCmd
                        SDVConfigured gcModel ->
                            Logic.startBattle (gcModel.p1a |> Array.toList |> armyFromList) (gcModel.p2a |> Array.toList |> armyFromList)
                            |> flip (Battle Nothing) core >> Scene
                            |> sansCmd
                        _ ->
                            phase |> sansCmd
                (GoBack, Scene (GameConfig confMod _)) ->
                    case confMod of
                        NothingConfigured ->
                            Scene (Title core)
                            |> sansCmd
                        SDVChosen gcModel ->
                            Scene (GameConfig NothingConfigured core)
                            |> sansCmd
                        SDVChosenOneName gcModel ->
                            Scene (GameConfig (SDVChosen gcModel) core)
                            |> sansCmd
                        SDVP1ChoosingPieces _ gcModel ->
                            Scene (GameConfig (SDVChosenOneName gcModel) core)
                            |> sansCmd
                        SDVP2ChoosingPieces _ gcModel ->
                            Scene (GameConfig (SDVP1ChoosingPieces 1 gcModel) core)
                            |> sansCmd
                        AIGameConfigured gcModel ->
                            Scene (GameConfig NothingConfigured core)
                            |> sansCmd
                        SDVConfigured gcModel ->
                            Scene (GameConfig (SDVP2ChoosingPieces 1 gcModel) core)
                            |> sansCmd
                (_, _) -> phase |> sansCmd
        TitlePS msg ->
            case msg of
                SelectNewGame ->
                    Scene (GameConfig NothingConfigured core)
                    --|> transitionWithAni 1000 [(PlayerFire 1, 0, 1000)]
                    |> sansCmd
                SelectOptions ->
                    (phase, Cmd.none)
                SelectCredits ->
                    (phase, Cmd.none)
                SelectTutorial ->
                    (phase, Cmd.none)
        BattlePS msg ->
            let
                bState = battleStateForPhase phase
                --passingTurn bState1 bState2 =
            in
            case msg of 
                SelectPiece spot ->
                    (Scene (Battle (Just spot) bState core), Cmd.none)
                MovePiece spot1 spot2 ->
                    (Scene (Battle Nothing (bState |> Logic.updateBattleState (Logic.Switch spot1 spot2)) core), Cmd.none)
                FirePiece spot ->
                    let
                        newBState = (bState |> Logic.updateBattleState (Logic.Fire spot))
                    in
                    Scene (Battle Nothing newBState core)
                    |> Transition 1000 (Scene (Cutscene [Graphics.shotAnim Near spot] core))
                    |> sansCmd
                DeselectPiece ->
                    (Scene (Battle Nothing bState core), Cmd.none)
                WatchRecap ->
                    (Scene (Battle Nothing bState core), Cmd.none)
                SkipRecap ->
                    (Scene (Battle Nothing bState core), Cmd.none)
                Terminate ->
                    (Scene (Battle Nothing bState core), Cmd.none)
        NoOp ->
            (phase, Cmd.none)
-- shotComplete = Transition 1000 (Cutscene playerShot core) (Transition 400 (Cutscene opponentDeath core) (Battle bMod core))
{-
        _ -> 
            case phase of
                Transition duration ((Transition duration2 p1 p2) as t) p3 ->
                    (phase, Cmd.none)
                Transition duration (Scene (Cutscene (current::remaining) core)) p2 ->
                    (phase, Cmd.none)
                Transition duration (Scene (Cutscene [] core)) p2 ->
                    (phase, Cmd.none)
                Scene (Title core) ->
                    case msg of
                Scene (GameConfig core gcModel) ->
                    (phase, Cmd.none)
                Scene (Battle core battleModel) ->
                    (phase, Cmd.none)
                _ ->
                    (phase, Cmd.none)

-}

view : Phase -> Html PhaseShift
view phase =
    let
        {width, height} = coreForPhase phase
        pageElements p = 
            case p of
                Scene (Title _) -> titleMenuView width height phase
                Scene (GameConfig gcm _) -> gcMenuView width height phase gcm
                Scene (Battle selected bState _) -> battleView width height phase selected bState
                Transition _ (Scene s) _ -> pageElements (Scene s)
                _ -> [ Html.text "Not implemented" ]
    in
    Html.main_
        []
        (pageElements phase)


getMenuForPhase phase =
    case phase of
        Scene (Title _) -> Just titleMenu
        Transition _ _ _ -> Nothing
        _ -> Nothing

subscriptions : Phase -> Sub PhaseShift
subscriptions phase =
    let 
        videoSubs = 
            [ Browser.Events.onResize SetWindowSize
            , Browser.Events.onAnimationFrameDelta (KeyFrameShift)
            ]
        inputSubs = 
            case phase of
                Scene (Title _) ->
                    [ Browser.Events.onKeyDown (titleMenuDecoder phase) ]
                Scene (GameConfig _ _) ->
                    [ Browser.Events.onKeyDown (gcMenuDecoder phase) ]
                Scene (Battle Nothing _ _) ->
                    [ Browser.Events.onKeyDown (battleMenuDecoder phase) ]
                Scene (Battle (Just spot) _ _) ->
                    [ Browser.Events.onKeyDown (battleMenuSelectedDecoder phase spot) ]
                Transition _ _ _ -> []
                _ -> []
    in
    Sub.batch (videoSubs ++ inputSubs)



battleTestPhase =
    let
        p1a =
            [ Logic.dishonestBlue
            , Logic.honestBlue
            , Logic.honestOrange
            , Logic.honestOrange
            , Logic.honestOrange
            ]
            |> Logic.armyFromList
        p2a =
            [ Logic.dishonestBlue
            , Logic.honestBlue
            , Logic.honestOrange
            , Logic.honestOrange
            , Logic.honestOrange
            ]
            |> Logic.armyFromList
        bState =
            Logic.startBattle p1a p2a
    in
    Scene
    ( Battle
        Nothing
        bState
        { width = 0
        , height = 0
        , config = initConfig
        , lastKey = ""
        }
    )


initialScene =
    Scene 
        ( Title 
            { width = 0
            , height = 0
            , config = initConfig
            , lastKey = ""
            }
        )


init : () -> ( Phase, Cmd PhaseShift )
init _ =
    ( battleTestPhase
    , Browser.Dom.getViewport |> Task.attempt (\r ->
        case r of
            Ok vp ->
                SetWindowSize (floor vp.viewport.width) (floor vp.viewport.height)
            Err e ->
                NoOp
        )
    )
--            case msg of



{-
                SelectPiece x
        let
                sansCmd m = (m, Cmd.none)
                withGameScene g m = {m | gameScene = g }
                sceneNone s = withGameScene s model |> sansCmd
                withAnim anis (mod, cmd) =
                    ({ mod | transitions = anis }
                    , cmd
                    )
                kbTitleCase spot =
                        case spot of
                                "1" -> sceneNone (Transition model (model |> withGameScene (GameConfig defaultGameConfigModel)) )
                                    |> withAnim [(PlayerFire 1, 0, 1000)] 
                                --"2" -> sceneNone (Tutorial TeachingMoving)
                                --"3" -> sceneNone Options
                                --"4" -> sceneNone Credits
                                _ -> (model, Cmd.none)
        
                mouseTitleCase menu spot =
                    (model.width |> toFloat)
                    / (menu |> optionsForMenu |> List.length |> toFloat)
                    |> floor 
                    |> (//) spot 
                    |> (+) 1
                    |> String.fromInt
                    |> kbTitleCase
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
                                        setNewBattleState m s =
                                            m |> withGameScene (Battle { bModel | battleState = s }) 
                                        setNewBattlePhase m p =
                                            m |> withGameScene (Battle { bModel | battlePhase = p })
                                        --mapBattleStateModel fn m =
                                            --m |> withGameScene (Battle (fn bModel))

                                        playerTurnIncrement turn playmode =
                                            if turn == First then
                                                PlayerTurn (Turn Second Nothing)
                                            else
                                                if playmode == SingleDeviceVs then
                                                    PassControl
                                                else
                                                    OpponentTurn
                                in
                                case (bMsg, bModel.battlePhase) of
                                    (SelectPiece p, PlayerTurn (Turn num (Just at))) ->
                                        let
                                            updatedModel = 
                                                Logic.updateBattleState event bModel.battleState
                                                |> setNewBattleState model
                                                |> flip setNewBattlePhase (playerTurnIncrement num bModel.playMode) 
                                            transitionNewModel event =
                                                Transition model updatedModel
                                        in
                                        if p /= at then
                                            sceneNone (transitionNewModel (Switch at p))
                                            |> withAnim [(PlayerMove at p, 0, 600)]
                    
                                        else 
                                            sceneNone (transitionNewModel (Fire at))
                                            |> withAnim [(PlayerFire at, 0, 1000)]

                                    (SelectPiece p, PlayerTurn (Turn t Nothing)) ->
                                        setNewBattlePhase model (PlayerTurn (Turn t (Just p)))
                                        |> sansCmd
                                    (_, PassControl) ->
                                        setNewBattlePhase model (PlayerTurn (Turn First Nothing))
                                        --|> mapBattleStateModel (\bs -> { bs | p1 = bs.p2, p2 = bs.p1 })
                                        |> sansCmd

                                    _ ->
                                        model |> sansCmd
                        Title ->
                                kbTitleCase spot
                        GameConfig gcModel ->
                                case gcModel of
                                        NothingConfigured ->
                                                case spot of
                                                        "1" -> sceneNone (GameConfig (AIGameConfigured (gameModelWithNames "Human" "Computer")))
                                                        "2" -> sceneNone (GameConfig (SDVChosen initGameModel))
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
                                        SDVP1ChoosingPieces (n1, p) n2 ->
                                                let
                                                    popts = placementOptions p
                                                in
                                                case (List.length p, List.length popts, spot) of 
                                                        (5, _, "1") -> sceneNone (GameConfig (SDVP2ChoosingPieces (n1, armyFromList p) (n2, [])))
                                                        (0, _, "5") -> sceneNone (GameConfig (SDVConfigured n1 n2))
                                                        (_, 4, "5") -> sceneNone (GameConfig (SDVP1ChoosingPieces (n1, List.take (List.length p - 1) p) n2))
                                                        (_, 2, "3") -> sceneNone (GameConfig (SDVP1ChoosingPieces (n1, List.take (List.length p - 1) p) n2))
                                                        (_, _, s) -> sceneNone 
                                                            (GameConfig (SDVP1ChoosingPieces 
                                                                (n1
                                                                , (Array.fromList popts
                                                                    |> Array.get (String.toInt s 
                                                                        |> Maybe.withDefault 1
                                                                        |> (+) -1) 
                                                                    |> Maybe.withDefault Obliterated) 
                                                                    |> List.singleton
                                                                    |> List.append p) n2))
                                        SDVP2ChoosingPieces (n1, a) (n2, p) ->
                                                let
                                                    popts = placementOptions p
                                                in
                                                case (List.length p, List.length popts, spot) of 
                                                        (5, _, "1") -> sceneNone 
                                                                (Battle 
                                                                    { player1 = n1
                                                                    , player2 = n2 
                                                                    , battleState = Logic.startBattle a (armyFromList p)
                                                                    , battlePhase = PlayerTurn (Turn First Nothing)
                                                                    , playMode = SingleDeviceVs
                                                                    }
                                                                )
                                                        (0, _, "5") -> sceneNone (GameConfig (SDVP1ChoosingPieces (n1, armyPiecesList a) n2))
                                                        (_, 4, "5") -> sceneNone (GameConfig (SDVP2ChoosingPieces (n1, a) (n2, List.take (List.length p - 1) p)))
                                                        (_, 2, "3") -> sceneNone (GameConfig (SDVP2ChoosingPieces (n1, a) (n2, List.take (List.length p - 1) p)))
                                                        (_, _, s) -> sceneNone 
                                                            (GameConfig (SDVP2ChoosingPieces 
                                                                (n1, a)
                                                                (n2 , (Array.fromList popts 
                                                                    |> Array.get (String.toInt s 
                                                                        |> Maybe.withDefault 0) 
                                                                    |> Maybe.withDefault Obliterated) 
                                                                    |> List.singleton
                                                                    |> List.append p)))
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
        KeyFrame time ->
            let
                timeExpired = (>) 0
            in
            case model.gameScene of
                (Transition currentGameScene nextGameScene) as transition ->
                    case model.transitions of 
                        ((current, delay, tRemaining)::next) ->
                            if delay - time |> timeExpired then
                                if tRemaining - time |> timeExpired then
                                    if List.length next > 0 then
                                        sceneNone transition
                                        |> withAnim next
                                    else
                                        (nextGameScene, Cmd.none)
                                else
                                    sceneNone transition
                                    |> withAnim ((current, 0, (tRemaining - time))::next)
                            else 
                                sceneNone transition
                                |> withAnim ((current, (delay - time), tRemaining)::next)
                        [] ->
                            nextGameScene |> sansCmd

                _ ->
                    (model, Cmd.none)
        _ ->
                (model, Cmd.none)
-}

                                                {-
defaultBattleModel : BattleModel
defaultBattleModel =
        { player1 = "player1"
        , player2 = "player2"
        , battleState = Logic.startBattle
            (Army 
                        (Piece Orange Authentic)
                        (Piece Blue Authentic)
                        (Piece Orange Deceitful)
                        (Piece Blue Deceitful)
                        (Piece Blue Authentic))
            (Army 
                        (Piece Orange Authentic)
                        (Piece Orange Authentic)
                        (Piece Orange Authentic)
                        (Piece Blue Authentic)
                        (Piece Blue Authentic))
 
        , battlePhase =  PlayerTurn (Turn First Nothing)
        , playMode = SingleDeviceVs 
        }

                                                
init : () -> ( Model, Cmd Msg)
init _ =
        (       { gameScene = 
                    --Title 
                    --GameConfig (SDVP1ChoosingPieces ("Alice", []) "Bob") 
                    Battle defaultBattleModel
                , config = initConfig
                , debugMode = True
                , width = 0
                , height = 0
                , transitions = []
                , helpOverlay = False
                }
                
        , Browser.Dom.getViewport |> Task.attempt (\r ->
                case r of
                        Ok vp ->
                                SetWindowSize (floor vp.viewport.width) (floor vp.viewport.height)
                        Err e ->
                                NoOp
                )
        )
                                                -}
main = Browser.element 
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions 
        }
