module Backend.Svg exposing (view)

import Game exposing (Game, Paddle, Position)
import Html exposing (Html)
import Math.Vector2 exposing (getX, getY)
import Model as M exposing (Model)
import Msg exposing (Msg(..))
import Svg as S exposing (Svg)
import Svg.Attributes as A
import Svg.Events as E


ball : Game -> Svg Msg
ball game =
    S.circle
        [ A.id "ball"
        , A.cx <| String.fromFloat <| getX game.position + M.paddleWidth
        , A.cy <| String.fromFloat <| getY game.position
        , A.r <| String.fromFloat <| M.halfBallWidth
        ]
        []


paddle : Paddle -> Position -> Svg Msg
paddle paddle_ position =
    S.rect
        [ A.class "paddle"
        , A.x <| String.fromFloat <| getX position
        , A.y <| String.fromFloat <| getY position
        , A.width <| String.fromFloat <| getX paddle_
        , A.height <| String.fromFloat <| getY paddle_
        ]
        []


view : Model -> Html Msg
view model =
    let
        width =
            String.fromFloat <| M.width + 2 * M.paddleWidth

        height =
            String.fromFloat <| M.height
    in
    S.svg
        [ A.id "container"
        , A.viewBox <| "0 0 " ++ width ++ " " ++ height
        , E.onClick MouseClick
        ]
        [ ball model.game
        , paddle model.game.paddle model.game.player1
        , paddle model.game.paddle model.game.player2
        ]
