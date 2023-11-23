module SameScreenTransitionTests exposing (..)

import Test exposing (Test, test, fuzz, only, describe, concat)
import Expect
import BattleLogic as BL
import Play exposing (SameScreenPhase(..))
import NoGFX
import Test.Html.Query as Query
import Test.Html.Selector as Selector
import Test.Html.Event as Event
import U
import Html
import Html.Attributes

honestBlue = BL.Piece BL.Blue BL.Authentic
dishonestBlue = BL.Piece BL.Blue BL.Deceitful
honestOrange = BL.Piece BL.Orange BL.Authentic

p1 = ( BL.initialPlayerModel BL.orangeArmy )
p2 = ( BL.initialPlayerModel BL.blueArmy )
p1n = "HORK"
p2n = "flan"
bState = BL.init p1 p2
b = BL.Battle bState ([], bState)
{-
newModel phase battle =
    Play.SameScreen phase (p1n, p2n) battle

swap = 
   Play.update (Play.BLMsg (BL.Switch 1 5))
   >> Tuple.first

transition =
    Play.update (Play.SameScreenTransition)
    >> Tuple.first

suite : Test
suite =
    [ transitionTests
    , privacyTests
    , mirrorTests
    ]
    |> concat 
    |> only 

privacyTests : Test
privacyTests =
    [ test "Lying units fly fallacious flags" 
        <| \_ ->
            BL.init p1 (BL.initialPlayerModel (BL.armyFromList (List.repeat 5 dishonestBlue)))
            |> U.duple 
            |> Tuple.mapSecond (Tuple.pair [])
            |> U.uncurry BL.Battle
            |> newModel PlayPhase
            |> NoGFX.playView
            |> Html.main_ []
            |> Query.fromHtml
            |> Query.find [ Selector.class "opponent" ]
            |> Query.findAll [ Selector.text "Blue" ]
            |> Query.count (Expect.equal 0)
    , test "Armies not swapped during review phase"
        <| \_ ->
            BL.init 
                (BL.initialPlayerModel (BL.armyFromList (List.repeat 5 honestBlue)))
                (BL.initialPlayerModel (BL.armyFromList (List.repeat 5 dishonestBlue)))
            |> U.duple 
            |> Tuple.mapSecond (Tuple.pair [])
            |> U.uncurry BL.Battle
            |> newModel PlayPhase
            |> swap
            |> swap
            |> NoGFX.playView
            |> Html.main_ []
            |> Query.fromHtml
            |> Query.find [ Selector.class "player" ]
            |> Query.findAll [ Selector.text "Deceitful" ]
            |> Query.count (Expect.equal 0)

    ]
    |> describe "Fog of war"

{-| This tests functionality that maybe is actually harmful to the user's
entertainment, fluidity of play, etc, so it is _skipped_ until we can get it
behind a non-default setting 
-}
mirrorTests : Test
mirrorTests  =
    [ test "P2 Turn, shoots mirrored spots (fire 1 is actually fire 5)"
        <| \_ ->
            newModel PlayPhase b
            |> swap
            |> swap
            |> transition
            |> transition
            |> NoGFX.playView
            |> Html.main_ []
            |> Query.fromHtml
            |> Query.find [ Selector.class "player" ]
            |> Query.findAll [ Selector.class "piece" ]
            |> Query.first
            |> Query.find [ Selector.class "fire-btn" ]
            |> Event.simulate Event.click
            |> Event.expect (Play.BLMsg (BL.Fire 5))
    , test "P2 Turn, movement mirrored (4 -> 2 is actually 2 -> 4)"
        <| \_ ->
            newModel PlayPhase b
            |> swap
            |> swap
            |> transition
            |> transition
            |> NoGFX.playView
            |> Html.main_ []
            |> Query.fromHtml
            |> Query.find [ Selector.class "player" ]
            |> Query.findAll [ Selector.class "piece" ]
            |> Query.first
            |> Query.find [ Selector.class "move-btns" ]
            |> Query.findAll [ Selector.tag "button" ]
            |> Query.index -2
            |> Event.simulate Event.click
            |> Event.expect (Play.BLMsg (BL.Switch 2 4))
    ]
    |> concat
    |> Test.skip
    |> List.singleton
    |> describe "Player2 row reversal"

transitionTests : Test
transitionTests =
    [ test "P1Phase, Second Turn, Act -> P1PhaseReview"
        <| \_ ->
            newModel PlayPhase b
            |> swap
            |> swap
            |> Expect.all
                [ \s -> case s of
                    Play.SameScreen ReviewPhase _ (BL.Battle {phase} _) -> 
                        if phase == BL.Player2 BL.First then Expect.pass else Expect.fail ("Wrong phase: " ++ (Debug.toString s))
                    _ -> Expect.fail ("Wrong phase: " ++ (Debug.toString s))
                , NoGFX.playView
                    >> Html.main_ []
                    >> Query.fromHtml
                    >> Query.find [Selector.tag "button"]
                    >> Event.simulate Event.click
                    >> Event.expect Play.SameScreenTransition
                ]
    , test "P1TransitionToP2, Accept -> P2Phase"
        <| \_ ->
            newModel PlayPhase b
            |> swap
            |> swap
            |> transition
            |> NoGFX.playView
            |> Html.main_ []
            |> Query.fromHtml
            |> Query.find [Selector.tag "button"]
            |> Event.simulate Event.click
            |> Event.expect Play.SameScreenTransition
    , test "P2Phase, Second Turn, Act -> P2PhaseReview"
        <| \_ ->
            newModel PlayPhase b
            |> swap
            |> swap
            |> transition
            |> transition
            |> swap
            |> swap
            |> NoGFX.playView
            |> Html.main_ []
            |> Query.fromHtml
            |> Query.find [Selector.tag "button"]
            |> Event.simulate Event.click
            |> Event.expect Play.SameScreenTransition
    , test "P2TransitionToP1, Accept -> P1Phase"
        <| \_ ->
            newModel PlayPhase b
            |> swap
            |> swap
            |> transition
            |> transition
            |> swap
            |> swap
            |> transition
            |> NoGFX.playView
            |> Html.main_ []
            |> Query.fromHtml
            |> Query.find [Selector.tag "button"]
            |> Event.simulate Event.click
            |> Event.expect Play.SameScreenTransition
    ]
    |> describe "Transitions between players"
-}
