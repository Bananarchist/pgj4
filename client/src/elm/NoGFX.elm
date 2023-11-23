module NoGFX exposing (..)
import BattleLogic

import Basics.Extra exposing (flip)
import BattleLogic exposing (BattleState(..))
import Maybe.Extra
import U
import Play as P
import Html exposing (Html)
import Html.Attributes as Hats
import Html.Events as Emit
import PiecePlacement as PP
import PlayerSetup as PS
import MainMenu as MM
import Server
import Settings as S
import Msg exposing (Msg)


ordinal : Int -> String
ordinal i =
    case i of
        1 ->
            "first"

        2 ->
            "second"

        3 ->
            "third"

        4 ->
            "fourth"

        5 ->
            "fifth"

        _ ->
            String.fromInt i ++ "th"


viewRecap : BattleState -> Bool -> BattleLogic.TurnRecap -> String
viewRecap bs isAttacker tr =
    let
        pieceDescriptor spot army =
            if isAttacker then
                [ BattleLogic.getHonestyAt spot army
                    |> Maybe.map BattleLogic.honestyString
                , BattleLogic.getHueAt True spot army
                    |> Maybe.map BattleLogic.hueString
                ]
                    |> Maybe.Extra.values
                    |> String.join " "

            else
                BattleLogic.getHueAt False spot army
                    |> Maybe.map BattleLogic.hueString
                    |> Maybe.withDefault ""
    in
    if isAttacker then
        case tr of
            BattleLogic.TurnEnded ->
                "Now act"

            BattleLogic.MovedToEmptySpot src dest ->
                String.join " "
                    [ "Moved"
                    , BattleLogic.player1Army bs |> pieceDescriptor dest
                    , "from"
                    , ordinal src
                    , "position to"
                    , ordinal dest
                    ]

            BattleLogic.SwappedPieces src dest ->
                String.join " "
                    [ "Swapped"
                    , BattleLogic.player1Army bs |> pieceDescriptor dest
                    , "in"
                    , ordinal src
                    , "position with"
                    , BattleLogic.player1Army bs |> pieceDescriptor src
                    , "in"
                    , ordinal dest
                    , "position"
                    ]

            BattleLogic.Damaged src ->
                "Fired for one damage!"

            BattleLogic.UnsuccessfulShot src ->
                String.join " "
                    [ BattleLogic.player1Army bs |> BattleLogic.getHueAt True src |> Maybe.map BattleLogic.hueString |> Maybe.withDefault ""
                    , "firing from"
                    , ordinal src
                    , "position was blocked!"
                    ]

            BattleLogic.SuccessfulShot src ->
                let
                    maybeHue =
                        BattleLogic.player1Army bs |> BattleLogic.getHueAt True src
                in
                String.join " "
                    [ maybeHue |> Maybe.map BattleLogic.hueString |> Maybe.withDefault ""
                    , "firing from"
                    , ordinal src
                    , "position obliterated opposing"
                    , maybeHue |> Maybe.map (BattleLogic.oppositeHue >> BattleLogic.hueString) |> Maybe.withDefault ""
                    ]
                    |> flip (++) "!"

            BattleLogic.GameEnded ->
                "Fired for one damage! Victory!"

    else
        case tr of
            BattleLogic.TurnEnded ->
                "Now wait and see"

            BattleLogic.MovedToEmptySpot src dest ->
                String.join " "
                    [ BattleLogic.player2Army bs |> pieceDescriptor dest
                    , "moved from"
                    , BattleLogic.translatedSpot src |> ordinal
                    , "position to"
                    , BattleLogic.translatedSpot dest |> ordinal
                    ]

            BattleLogic.SwappedPieces src dest ->
                String.join " "
                    [ BattleLogic.player2Army bs |> pieceDescriptor dest
                    , "in"
                    , BattleLogic.translatedSpot src |> ordinal
                    , "position swapped places with"
                    , BattleLogic.player2Army bs |> pieceDescriptor src
                    , "in"
                    , BattleLogic.translatedSpot dest |> ordinal
                    , "position"
                    ]

            BattleLogic.UnsuccessfulShot src ->
                String.join " "
                    [ BattleLogic.player2Army bs |> pieceDescriptor src
                    , "in"
                    , BattleLogic.translatedSpot src |> ordinal
                    , "position evaded obliteration!"
                    ]

            BattleLogic.SuccessfulShot src ->
                let
                    maybeHue =
                        BattleLogic.player2Army bs |> BattleLogic.getHueAt True src
                in
                String.join " "
                    [ maybeHue |> Maybe.map BattleLogic.hueString |> Maybe.withDefault ""
                    , "firing from"
                    , BattleLogic.translatedSpot src |> ordinal
                    , "position obliterated opposing"
                    , maybeHue |> Maybe.map (BattleLogic.oppositeHue >> BattleLogic.hueString) |> Maybe.withDefault ""
                    ]
                    |> flip (++) "!"

            BattleLogic.Damaged src ->
                "Took one damage!"
            BattleLogic.GameEnded ->
                "Took one damage and a defeat! You lost!"


view : BattleState -> List String
view bs =
    let
        viewAttackerRecap =
            viewRecap bs True

        viewSpectatorRecap =
            viewRecap bs False
    in
    case bs of
        Battle { phase } (recap, _) ->
            case phase of
                BattleLogic.Player1 _ ->
                    List.map viewAttackerRecap recap

                BattleLogic.Player2 _ ->
                    List.map viewSpectatorRecap recap

        _ ->
            [ "" ]

mainMenuView : List (Html MM.Msg)
mainMenuView =
    [ MM.StartLocalGame
    , MM.AdjustSettings
    , MM.ConnectToNetwork
    ]
    |> List.map 
        ( U.duple 
        >> Tuple.mapBoth 
            (Emit.onClick >> List.singleton) 
            (MM.menuText >> Html.text >> List.singleton)
        >> U.uncurry Html.button
        )

viewPiece : Bool -> BattleLogic.Piece -> String
viewPiece withHonesty =
    U.duple
    >> Tuple.mapBoth 
        ( BattleLogic.getHonestyOf 
            >> Maybe.map BattleLogic.honestyString 
            >> Maybe.map (++)) 
        ( BattleLogic.getHueOf withHonesty 
            >> Maybe.map BattleLogic.hueString)
    >> U.maybeTuple
    >> Maybe.map (\(t, f) -> U.bool (t f) f withHonesty)
    >> Maybe.withDefault "Undefended!"


viewSelection : List BattleLogic.Piece -> List String
viewSelection =
    List.indexedMap (always (viewPiece True))

pieceSelectorList : List (BattleLogic.Honesty, BattleLogic.Hue)
pieceSelectorList =
    [ (BattleLogic.Authentic, BattleLogic.Orange)
    , (BattleLogic.Authentic, BattleLogic.Blue)
    , (BattleLogic.Deceitful, BattleLogic.Orange)
    , (BattleLogic.Deceitful, BattleLogic.Blue)
    ]

viewPieceSelector : List String
viewPieceSelector =
    pieceSelectorList
    |> List.map (Tuple.mapBoth BattleLogic.honestyString BattleLogic.hueString >> U.tupleList >> String.join " ")


{-

lobbyView : LB.Model -> List (Html LB.Msg)
lobbyView =
    List.repeat 3
    >> List.map2 (<|) [ lobbyChatView, lobbyGameListView, lobbyUserListView ]

lobbyChatView : LB.Model -> Html LB.Msg
lobbyChatView lmod =
    List.map lobbyChatMessageView lmod.chatMsgs 
    ++ [ Html.form []
        [ Html.fieldset []
            [ Html.label [ Hats.for "chat-input" ] [ Html.text ">" ]
            , Html.input [ Hats.id "chat-input", Emit.onInput LB.InputChat ] []
            ]
        ]
    ]
    |> Html.section []
    
lobbyChatMessageView : LB.ChatMsg -> (Html LB.Msg)
lobbyChatMessageView msg =
    Html.article [ Hats.class "chat" ]
        [ Html.time [] [ Html.text msg.timestamp ]
        , Html.span [] [ Html.text msg.user.username ]
        , Html.p [] [ Html.text msg.message ]
        ]

lobbyGameListView : LB.Model -> Html LB.Msg
lobbyGameListView lmod =
    Html.button [ Hats.id "New Game" ] [] --, Emit.onClick LB. ] []
    :: List.map lobbyGameView lmod.gameList
    |> Html.section []
    
lobbyGameView : Server.Game -> Html LB.Msg
lobbyGameView game =
    Html.span [] [ Html.text game ]


lobbyUserListView : LB.Model -> Html LB.Msg
lobbyUserListView lmod =
    List.map lobbyUserView lmod.userList
    |> Html.section []
    
lobbyUserView user =
    Html.span [] [ Html.text user.username ]

-}

playerSetupViewInit : PS.Model -> List (Html PS.Msg)
playerSetupViewInit ps =
    let
        returnBtn = [ Html.button [ Emit.onClick PS.ReturnToMenu, Hats.id "return-btn" ] [ Html.text "Return" ] ]
        backBtn = [ Html.button [ Emit.onClick (PS.PlayerModelMsg PS.GoBack), Hats.id "back-btn" ] [ Html.text "Back" ] ]
        nextBtn = [ Html.button [ Emit.onClick (PS.PlayerModelMsg PS.Confirm), Hats.id "confirm-btn" ] [ Html.text "Confirm" ] ]
        startBtn = [ Html.button [ Emit.onClick PS.StartGame, Hats.id "start-btn" ] [ Html.text "Start Game" ] ]
    in
    case (PS.exitableStateか ps, PS.completeか ps) of
        (True, True) -> returnBtn ++ nextBtn ++ startBtn
        (True, False) -> returnBtn ++ nextBtn
        (False, True) -> backBtn ++ nextBtn ++ startBtn
        (False, False) -> backBtn ++ nextBtn

playerSetupView : PS.Model -> List (Html PS.Msg)
playerSetupView ps =
    let
        psv = playerSetupViewInit ps
    in
    case (PS.transitioningか ps, PS.activePlayerModel ps) of
        (True, Just (PS.Named s _)) ->
            playerSetupTransitionView psv s
        (True, Nothing) ->
            playerSetupTransitionView psv "Player 2"
        (False, Just PS.Unnamed) ->
            playerSetupNamingView psv ""
        (False, Just (PS.Naming s _)) ->
            playerSetupNamingView psv s
        (False, Just (PS.Named s a)) -> 
            playerSetupPiecePlacementView psv s a
        _ ->
            [ Html.b [] [ Html.text "This should not have happened" ] ]

playerSetupTransitionView : List (Html PS.Msg) -> String -> List (Html PS.Msg)
playerSetupTransitionView psv name =
    Html.p [] [ Html.text ("Please avert your eyes and surrender control to " ++ name) ]
    :: psv
    

playerSetupNamingView : List (Html PS.Msg) -> String -> List (Html PS.Msg)
playerSetupNamingView psv name =
     Html.fieldset []
        [ Html.label [ Hats.for "player-name" ] [ Html.text "Name: " ]
        , Html.input [ Hats.id "player-name", Emit.onInput PS.setName ] []
        ]
    :: psv

playerSetupPiecePlacementView : List (Html PS.Msg) -> String -> PP.Model -> List (Html PS.Msg)
playerSetupPiecePlacementView psv name army =
    Html.p [] [ Html.text ("Build your army of five, " ++ name) ]
    :: playerSetupPiecePlacementSelectionView army
    ++ ( if (PP.pieceCount army) == 5 then 
            [ ] 
        else 
            playerSetupPieceSelectorView
        )
    ++ psv

playerSetupPiecePlacementSelectionView : PP.Model -> List (Html PS.Msg)
playerSetupPiecePlacementSelectionView =
    PP.selectedPieces
    >> viewSelection
    >> List.map (U.singletonText >> Html.div [ Hats.class "piece" ])

playerSetupPieceSelectorView : List (Html PS.Msg)
playerSetupPieceSelectorView =
    List.map2 (\(honesty, hue) -> 
        U.singletonText >> Html.button [ Hats.class "piece", Emit.onClick (PP.AddPiece hue honesty |> PS.setPiece) ] )
        pieceSelectorList
        viewPieceSelector

playView : P.Model -> List (Html P.Msg)
playView p =
    [ Html.text "not implemented" ]
    {-
    case p of
        P.SameScreen P.Initial (p1n, _) state ->
            playTransitionScreenView p1n
        P.SameScreen P.PlayPhase names state ->
            playSameScreenView P.PlayPhase names state 
        P.SameScreen P.ReviewPhase names state ->
            playSameScreenView P.ReviewPhase names state
        P.SameScreen P.TransitionPhase (p1n, p2n) state ->
            if BattleLogic.player1Turnか state then
                playTransitionScreenView p1n
            else
                playTransitionScreenView p1n
        P.SameScreen P.Recap names state ->
            playScreenRecapView names state
        P.Network _ _ ->
            [ Html.p [] [ Html.text "Unimplemented..." ] ]
        P.AI state ->
            [ Html.p [] [ Html.text "Unimplemented..." ] ]


playTransitionScreenView : String -> List (Html P.Msg)
playTransitionScreenView name =
    [ Html.p [] [ Html.text ("Please avert your attention and surrender control to " ++ name) ]
    , Html.button [ Emit.onClick P.SameScreenTransition ] [ Html.text "Proceed" ]
    ]

playSameScreenView : P.SameScreenPhase -> (String, String) -> BattleLogic.BattleState -> List (Html P.Msg)
playSameScreenView phase names state =
    let
        (playerArmyView, opponentArmyView) =
            case (phase, BattleLogic.player1Turnか state) of
                (P.PlayPhase, True) -> (BattleLogic.player1Army >> playScreenUsingArmy False, BattleLogic.player2Army >> playScreenInterpretingArmy)
                (P.PlayPhase, False) -> (BattleLogic.player2Army >> playScreenUsingArmy False, BattleLogic.player1Army >> playScreenInterpretingArmy)
                (P.ReviewPhase, False) -> (BattleLogic.player1Army >> playScreenReviewingArmy, BattleLogic.player2Army >> playScreenInterpretingArmy)
                (P.ReviewPhase, True) -> (BattleLogic.player2Army >> playScreenReviewingArmy, BattleLogic.player1Army >> playScreenInterpretingArmy)
                _ -> (BattleLogic.player1Army >> playScreenUsingArmy False, BattleLogic.player2Army >> playScreenInterpretingArmy) |> Debug.log "called inappropriately"
        continueBtn =
            case phase of
                P.ReviewPhase -> [ Html.button [ Emit.onClick P.SameScreenTransition ] [ Html.text "Continue" ] ]
                _ -> []
    in
    playScreenRecapView names state
    ++ playScreenHealthView names state
    ++ opponentArmyView state
    ++ playerArmyView state
    ++ continueBtn
    


playScreenOpponentArmyView : BattleLogic.Army -> List (Html P.Msg)
playScreenOpponentArmyView =
    BattleLogic.armyPiecesList
    >> List.map (viewPiece False >> U.singletonText >> Html.p [] >> List.singleton >> Html.div [ Hats.class "piece" ])
    >> Html.div [ Hats.classList [("army", True), ("opponent", True) ] ]
    >> List.singleton

playScreenPieceView : Bool -> Int -> BattleLogic.Piece -> List (Html P.Msg)
playScreenPieceView flipped spot piece =
    if piece == BattleLogic.Obliterated then
        [ Html.div [ Hats.class "piece" ] [ Html.p [] [ Html.text (viewPiece True piece) ] ] ]
    else
        let     
            rotateSpot v = 
                if flipped then
                    BattleLogic.translatedSpot v
                else
                    v
            realSpot = rotateSpot spot
            fireBtnAttrs =
                flip (::) [ Hats.class "fire-btn" ] 
            moveButtons = 
                List.range 1 5 
                |> List.map (\s -> Html.button 
                    [
                        (if s == realSpot then 
                            Hats.disabled True 
                        else  
                            Emit.onClick 
                                (P.BLMsg 
                                    (BattleLogic.Switch realSpot 
                                        (rotateSpot s))))]
                    [ Html.text (String.fromInt s) ] )
        in
        [ Html.p [] [ Html.text (viewPiece True piece) ]
        , Html.fieldset []
            [ Html.button 
                ( fireBtnAttrs <| Emit.onClick 
                    ( realSpot 
                    |> BattleLogic.Fire 
                    |> P.BLMsg) 
                ) 
                [ Html.text "Fire!" ]
            , Html.fieldset [ Hats.class "move-btns" ]
                ( Html.span [] [ Html.text "Move to " ]
                :: moveButtons 
                )
            ]
        ]
        |> Html.div [ Hats.class "piece" ] 
        |> List.singleton

playScreenUsingArmy : Bool -> BattleLogic.Army -> List (Html P.Msg) 
playScreenUsingArmy flipped = 
    BattleLogic.armyPiecesList
    >> List.foldr (\v (idx, acc) -> (idx - 1, playScreenPieceView flipped idx v ++ acc)) (5, [])
    >> Tuple.second
    >> Html.div [ Hats.classList [("army", True), ("player", True) ] ]
    >> List.singleton


playScreenReviewingArmy : BattleLogic.Army -> List (Html P.Msg) 
playScreenReviewingArmy =
    BattleLogic.armyPiecesList
    >> List.map (viewPiece True >> U.singletonText >> Html.p [] >> List.singleton >> Html.div [ Hats.class "piece" ])
    >> Html.div [ Hats.classList [("army", True), ("player", True) ] ]
    >> List.singleton

playScreenInterpretingArmy : BattleLogic.Army -> List (Html P.Msg) 
playScreenInterpretingArmy =
    BattleLogic.armyPiecesList
    >> List.map (viewPiece False >> U.singletonText >> Html.p [] >> List.singleton >> Html.div [ Hats.class "piece" ])
    >> Html.div [ Hats.classList [("army", True), ("opponent", True) ] ]
    >> List.singleton


playScreenHealthView : (String, String) -> BattleState -> List (Html P.Msg)
playScreenHealthView names bs =
    [ Html.p [ ] [ Html.text (Tuple.first names ++ ": " ++ (BattleLogic.player1Health bs |> String.fromInt)) ]
    , Html.p [ ] [ Html.text (Tuple.second names ++ ": " ++ (BattleLogic.player2Health bs |> String.fromInt)) ]
    ]

playScreenRecapView : (String, String) -> BattleState -> List (Html P.Msg)
playScreenRecapView names bs =
    let 
        (recap, _) = BattleLogic.recap bs
    in
    if List.length recap > 0 then
        List.foldl (viewRecap bs True >> U.singletonText >> Html.p [] >> (::)) [] recap
    else
        [ Html.p [] [ Html.text "Let the game begin!" ] ]
-}
