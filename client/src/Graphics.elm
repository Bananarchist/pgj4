module Graphics exposing (..)

import Svg exposing (Svg)
import Svg.Attributes
import Svg.Events
import Html exposing (Html)
import Cylinder3d
import Html.Attributes
import Basics.Extra exposing (flip) 
import Html.Events
import Time
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
import Logic exposing (armyPiecesList, Spot, Color(..), Piece(..), Army, updatePieceAt, getPieceAt, Honesty(..))
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


type Distance
    = Near
    | Far

type Placement 
    = Wall
    | Floor

type Choreography
    = Battle 
    | NearFire Spot
    | FarFire Spot
    | PlayerMove Spot Spot
    | OpponentMove Spot Spot
    | PlayerDamage
    | OpponentDamage
    | PlayerDeath Spot
    | OpponentDeath Spot


type Mutation
    = Placed Spot
    | Aligned Distance
    | Swap Spot Spot
    --| Dying
    --| Firing

type SpotEntity 
    = NullEntity
    | PieceSprite Color Honesty
    | HealthBlock
    | DeathBlock
    | TextEntity String
    | SelectedPiece
    | PlacementPiece
    | Beam Color

type alias Timing =
    { delay : Float
    , duration : Float
    }

type Animation 
    = Animation Delay Choreography Duration


withOpponentKillShot : Spot -> Engine -> Engine 
withOpponentKillShot spot engine =
    { engine
        | anis = [ playerFire spot, opponentDeath spot ]
    }

-- update...
engine
|> withOpponentKillShot 1
|> model
|> sansCmd

-- view...
engine
|> withScene (Just army1) (Just army2) (Just 4) (Just 5)
|> toEntities

withScene 
    : Maybe Army 
    -> Maybe Army
    -> Maybe Int
    -> Maybe Int
    -> Engine
    -> Engine
withScene armyNear armyFar healthNear healthFar engine =
    { engine
        | scene = { nearArmy = armyNear, farArmy = armyFar, nearHealth = healthNear, farHealth = healthFar }
    }

toEntities : ProcessingScene WithoutEntities -> List 
toEntities engine =
    engine.nearArmy
    |> Maybe.map armyToSpotEntities
    |> 

type alias Scene =
    { nearArmy : Maybe Army
    , farArmy : Maybe Army
    , nearHealth : Maybe Int
    , farHealth : Maybe Int
    --, text??
    }

genEntities
case battle
army |> entities

type alias Engine =
    { entities : List SpotEntity
    , scene : Scene
    , width : Int
    , height : Int
    }

type Mutant
    = Mutant SpotEntity (List Mutation)


spotEntity se =
    case se of 
        NullEntity ->
            Scene3d.group []
        PieceSprite color honesty ->
            pieceView (Piece color honesty)
        HealthBlock ->
            Scene3d.quad (Material.emissive Scene3d.Light.daylight (Luminance.nits 300))
            |> healthBlock 
        DeathBlock -> 
            Scene3d.quad (Material.color StdColor.red)
            |> healthBlock
        TextEntity str ->
            
        SelectedPiece ->
        PlacementPiece ->
        Beam colr ->


mutation (Mutant se mutations) =
    spotEntity se
    |> applyMutations mutations

applyMutations mutations entities =
    List.foldl (\e f -> applyMutation f e) entities mutations

applyMutation mutation entities =
    case mutation of
        Location spot distance ->
            entities
            |> p2faceoff distance
            |> spotTranslation spot
            |> battleSpace distance
        Swap s1 s2 ->
            entities 
            |> movingAnimation s1 s2 -- moving animation will decide to go up/down based on (s1-s2), so animation call should swap per entity
        Dying ->
            entities


theMutants |> List.map mutants applyMutation

initBattle =
    mutants =
        army1 |> armyToSpotEntities 
        army2 |> armyToSpotEntities



armyToSpotEntities army distance =
    let
        asPieceMutants = List.map (asPieceSprite >> (flip Mutant []))
    in
    army
    |> Logic.armyPiecesList
    |> asPieceMutants
    |> List.indexedMap (withPlacementMutation >> withAlignmentMutation distance)

withPlacementMutation spot (Mutant se mutations) =
    Mutant se ((Placed spot)::mutations)

withAlignmentMutation distance (Mutant se mutations) =
    Mutant se ((Aligned distance)::mutations)

asPieceSprite piece =
    case piece of 
        Obliterated -> 
            NullEntity
        Piece color honesty ->
            PieceSprite color honesty


aniNextFrame delta (Animation delay c duration) = 
    if delay <= 0
        Animation 0 c (duration - delta)
    else 
        Animation (delay - delta) c duration

nextFrame : Float -> Engine -> Engine
nextFrame delta engine =
    { engine 
        | entities = 
            engine.entities
                |> List.map 

army
|> maybe.map armyToSpotEntities
|> maybe.map applyAnimationsToEntities

mutantSpotIs spot maybeMutant =
    Maybe.map (\m -> if spotForMutant m == spot then maybeMutant else Nothing)

mutantDistanceIs distance maybeMutant =
    Maybe.map (\m -> if distanceForMutant m == distance then maybeMutant else Nothing)

mutantWithFiring (Mutant se mutations) =
    Firing |> List.singleton |> (++) mutations |> Mutant se

mutantDistanceSpotIs distance spot maybeMutant =


mutantIsOrMaybe qualifier1 qualifier2 maybeMutant =

applyAnimationToMutant (Animation delay c duration) ((Mutant se mutations) as mutant) =
    if delay > 0 then
        mutant
    else 
        case (c, se) of
            (NearFire spot, PieceSprite _ _) -> 
                Just mutant
                |> Maybe.andThen (mutantSpotIs spot)
                |> Maybe.andThen (mutantDistanceIs Near)
                |> Maybe.map mutantWithFiring
                |> Maybe.withDefault mutant
            (FarFire spot, PieceSprite _ _) -> 
                Just mutant
                |> Maybe.andThen (mutantSpotIs translatedSpot)
                |> Maybe.andThen (mutantDistanceIs Far)
                |> Maybe.map mutantWithFiring
                |> Maybe.withDefault mutant
            (NearMove from to, PieceSprite _ _) -> 
                Just mutant
                |> Maybe.andThen (mutantDistanceIs Far)
                |> Maybe.andThen 
                if distanceForMutant mutant = Far then
                    if 
                if spotForMutant mutant == spot1 && distanceForMutant mutant == Far then
                    Mutant se Firing
                else
                    mutant




translateSpot spot =
    case spot of
        1 -> 5
        2 -> 4
        4 -> 2
        5 -> 5
        _ -> spot

spotForMutant (Mutant se mutations) =
    case mutations of
        [] -> Nothing
        m :: ms -> 
            case m of
                Placed spot -> spot
                _ -> spotForMutant (Mutant se ms)

distanceForMutant (Mutant se mutations) =
    case mutations of
        [] -> Nothing
        m :: ms -> 
            case m of
                Aligned distance -> distance
                _ -> distanceForMutant (Mutant se ms)



moveAnim distance spot1 spot2 =
    case distance of
        Near -> (PlayerMove spot1 spot2, 0, 1000)
        Far -> (OpponentMove spot1 spot2, 0, 700)

shotAnim distance spot1 =
    case distance of
        Near -> (NearFire spot1, 0, 1000)
        Far -> (FarFire spot1, 0, 700)


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

p2faceoff row =
    case row of
        Far ->
            Scene3d.rotateAround Axis3d.z (Angle.degrees 180)
        _ -> identity

spotHighlighter spot =
        Scene3d.quad (Material.emissive (Scene3d.Light.color StdColor.yellow) (Luminance.nits 10000)) --Scene3d.Light.fluorescent (Luminance.nits 10000))
            (Point3d.meters -1 -1 -1)
            (Point3d.meters 1 -1 -1)
            (Point3d.meters 1 1 -1)
            (Point3d.meters -1 1 -1)
        |> Scene3d.scaleAbout Point3d.origin 1.1
        |> p2faceoff Near
        |> spotTranslation spot
        |> Scene3d.translateBy (Vector3d.meters 0.65 0 0)

battleSpotHighlighter spot =
        Scene3d.quad (Material.emissive (Scene3d.Light.color StdColor.purple) (Luminance.nits 100)) --Scene3d.Light.fluorescent (Luminance.nits 10000))
            (Point3d.meters -10 -1.1 -1.01)
            (Point3d.meters 10 -1.1 -1.01)
            (Point3d.meters 10 1.1 -1.01)
            (Point3d.meters -10 1.1 -1.01)
        --|> Scene3d.scaleAbout Point3d.origin 1.1
        |> p2faceoff Near
        |> spotTranslation spot
        |> Scene3d.translateBy (Vector3d.meters 0.65 0 0)

spotTranslation : Int -> Scene3d.Entity coordinates -> Scene3d.Entity coordinates
spotTranslation spot =
        Scene3d.translateBy
                (Vector3d.meters 0 (spot - 2 |> toFloat |> (*) 2.1) 0)

armyFarView = 
        armyPiecesList >> List.reverse >> List.indexedMap (entityView Far) 

armyNearView =
        armyPiecesList >> List.indexedMap (entityView Near)

entityView row spot piece =
        pieceView piece
                |> p2faceoff row
                |> spotTranslation spot
                |> battleSpace row

battleSpace row =
        case row of
               Far -> 
                        Scene3d.translateBy
                                (Vector3d.meters -8.7 0 0)
               _ ->
                        Scene3d.translateBy
                                (Vector3d.meters 0.8 0 0)


scene3dText : Int -> Int -> List String -> Int -> String -> Svg msg
scene3dText width height classes spot string =
        let
                --spot = 5 - s
                screenRect = 
                        Rectangle2d.from Point2d.origin (Point2d.pixels (toFloat width) (toFloat height))

                                
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
                ((List.map Svg.Attributes.class classes)
                ++
                [ Svg.Attributes.x x
                , Svg.Attributes.y y
                , Svg.Attributes.transform ("rotate(-90 " ++ x ++ " " ++ y ++") translate(0 " ++ (height // 100 * 8 |> String.fromInt ) ++ ")")
                ] )
                [ Svg.text string ]


moveAnimation : Bool -> Army -> Spot -> Spot -> Float -> List (Scene3d.Entity coordinates)
moveAnimation isPlayer army spot1 spot2 percentComplete =
    --move along x
    --move along y
    --move back along x
    let
        staticArmy = 
            army
            |> updatePieceAt spot1 Obliterated
            |> updatePieceAt spot2 Obliterated
        dancer1 = 
            army |> getPieceAt spot1
        dancer2 = 
            army |> getPieceAt spot2
        
        animateJourney negation = 
            let
                sign = negation |> toFloat
            in
            if percentComplete < 0.3 then
                -- 3, 4.2, 3
                Scene3d.translateBy (Vector3d.meters (sign * percentComplete * 10.2) 0 0)
            else if percentComplete < 0.70 then
                Scene3d.translateBy (Vector3d.meters (sign*3) (sign * (percentComplete * 10.2) - 3) 0)
            else 
                Scene3d.translateBy (Vector3d.meters (percentComplete * 10.2) (sign*4)  0)
        

        entities = 
            if isPlayer then
                armyNearView staticArmy
                ++
                    ( [dancer1, dancer2]
                        |> List.map (pieceView >> (spotTranslation spot1) >> (p2faceoff Near))
                        |> List.indexedMap (\i p -> animateJourney (i * 2 - 1) p)
                    )

            else
                armyFarView staticArmy
                ++
                    ( [dancer1, dancer2]
                        |> List.map (pieceView >> (spotTranslation spot1) >> (p2faceoff Far))
                        |> List.indexedMap (\i p -> animateJourney (i * 2 - 1) p)
                    )
    in
    entities
                

shotAnimation : Bool -> Color -> Spot -> Float -> List (Scene3d.Entity coordinates)
shotAnimation isPlayer color spot percentComplete =
    let
        entities = 
            [ Scene3d.cylinder (Material.emissive (Scene3d.Light.color StdColor.orange) (Luminance.nits 200)) 
                    ( Cylinder3d.along Axis3d.x
                        { start = Length.meters -9
                        , end = Length.meters (10 * percentComplete - 10) 
                        , radius = Length.centimeters 35
                        }
                    )
                |> spotTranslation spot
                |> Scene3d.translateBy (Vector3d.meters 0 0 2)
            ]
    in
    entities


pieceView : Piece -> Scene3d.Entity coordinates
pieceView piece =
        let
            cubeSize = 2
        in
        case piece of
                Obliterated ->
                        Scene3d.group []
                Piece color honesty ->
                        let
                                (frontColor, backColor) = 
                                        case (color, honesty) of
                                                (Orange, Authentic) ->
                                                        (StdColor.orange, StdColor.orange)
                                                (Orange, Deceitful) ->
                                                        (StdColor.blue, StdColor.orange)
                                                (Blue, Authentic) ->
                                                        (StdColor.blue, StdColor.blue)
                                                (Blue, Deceitful) ->
                                                        (StdColor.orange, StdColor.blue)
                                        
                        in
                        Scene3d.group
                                [ Scene3d.quad (Material.metal { baseColor = StdColor.grey, roughness = 1.0 })
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
                                , Scene3d.quad (Material.metal { baseColor = StdColor.grey, roughness = 1.0 })
                                    (Point3d.meters -1 -1 -1)
                                    (Point3d.meters -1 -1 1)
                                    (Point3d.meters 1 -1 1)
                                    (Point3d.meters 1 -1 -1)
                                    --slightly right
                                , Scene3d.quad (Material.metal { baseColor = StdColor.grey, roughness = 1.0 })
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
