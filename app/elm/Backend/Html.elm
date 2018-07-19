module Backend.Html exposing (view)

import Game exposing (Game, Paddle, Player(..))
import Html as H exposing (Html)
import Html.Attributes as A
import Html.Events as E
import Math.Vector2 exposing (vec2, getX, getY)
import Model as M exposing (Model)
import Msg exposing (Msg(..))
import Mouse exposing (Position)


width : Float
width =
    400.0


height : Float
height =
    300.0


ball : Game -> Html Msg
ball game =
    H.div
        [ A.id "ball"
        , A.style
            [ ( "border-width", (toString M.halfBallWidth) ++ "px" )
            , ( "border-radius", (toString M.halfBallWidth) ++ "px" )
            , ( "top", (getY game.position - M.halfBallHeight |> toString) ++ "px" )
            , ( "left", (getX game.position - M.halfBallWidth |> toString) ++ "px" )
            , ( "width", (toString M.ballWidth) ++ "px" )
            , ( "height", (toString M.ballHeight) ++ "px" )
            ]
        ]
        []


paddle : Float -> Paddle -> Html Msg
paddle x ( y, height ) =
    H.div
        [ A.class "paddle"
        , A.style
            [ ( "width", (toString M.paddleWidth) ++ "px" )
            , ( "height", (toString height) ++ "px" )
            , ( "top", (toString <| y + M.halfBallHeight) ++ "px" )
            , ( "left", (toString x) ++ "px" )
            ]
        ]
        []


board : Game -> Html Msg
board game =
    H.div
        [ A.id "board"
        , A.style
            [ ( "width", (toString width) ++ "px" )
            , ( "height", (toString height) ++ "px" )
            , ( "top", (toString M.halfBallHeight) ++ "px" )
            , ( "left", (toString <| M.paddleWidth + M.halfBallWidth) ++ "px" )
            ]
        ]
        [ ball game
        ]


view : Model -> Html Msg
view model =
    H.div
        [ A.id "container"
        , A.style
            [ ( "width", (toString <| width + M.ballWidth + 2 * M.paddleWidth) ++ "px" )
            , ( "height", (toString <| height + M.ballHeight) ++ "px" )
            ]
        , E.onClick MouseClick
        ]
        [ board model.game
        , paddle 0 model.game.player1
        , paddle (M.paddleWidth + getX model.game.board + M.ballWidth) model.game.player2
        ]
