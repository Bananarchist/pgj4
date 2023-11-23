module PlayerSetupTests exposing (suite)

import Test exposing (Test, test, fuzz, only, describe, concat, todo)
import Expect
import BattleLogic as BL
import NoGFX
import PlayerSetup as PS
import Test.Html.Query as Query
import Test.Html.Selector as Selector
import Test.Html.Event as Event
import PiecePlacement as PP
import U
import Html
import Html.Attributes

honestBlue = BL.Piece BL.Blue BL.Authentic
dishonestBlue = BL.Piece BL.Blue BL.Deceitful
honestOrange = BL.Piece BL.Orange BL.Authentic

p1 = PS.Named p1n (PP.Complete [] BL.blueArmy )
p2 = PS.Named p2n (PP.Complete [] BL.orangeArmy )
p1n = "HORK"
p2n = "flan"

suite : Test
suite =
    [ transitionTests
    --, privacyTests
    ]
    |> concat 

transitionTests : Test
transitionTests =
    [ test "There is a button for returning to menu"
        <| \_ ->
            PS.initSameScreen
            |> NoGFX.playerSetupView
            |> Html.main_ []
            |> Query.fromHtml
            |> Query.find [ Selector.id "return-btn" ]
            |> Event.simulate Event.click
            |> Event.expect PS.ReturnToMenu
    , test "No back button when there is a menu return button"
        <| \_ ->
            PS.initSameScreen
            |> NoGFX.playerSetupView
            |> Html.main_ []
            |> Query.fromHtml
            |> Query.findAll [ Selector.id "back-btn" ]
            |> Query.count (Expect.equal 0)

    , test "There is a button for starting the game"
        <| \_ ->
            PS.SameScreen2 p1 p2
            |> NoGFX.playerSetupView
            |> Html.main_ []
            |> Query.fromHtml
            |> Query.find [ Selector.id "start-btn" ]
            |> Event.simulate Event.click
            |> Event.expect PS.StartGame
    , test "Start Game does in fact start game"
        <| \_ ->
            PS.update PS.StartGame (PS.SameScreen2 p1 p2)
            |> (\(m, _) ->
                case m of
                    PS.StartingSameScreenGame _ _ -> Expect.pass
                    _ -> Expect.fail (Debug.toString m)
                )

    ]
    |> describe "Transitioning to/from menu/game"

privacyTests : Test
privacyTests =
    [ todo "Lying units fly fallacious flags" 
    ]
    |> describe "Fog of war"
