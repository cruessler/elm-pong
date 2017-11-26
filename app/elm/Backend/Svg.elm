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
        , A.cx <| toString <| getX game.position + M.paddleWidth
        , A.cy <| toString <| getY game.position
        , A.r <| toString <| M.halfBallWidth
        ]
        []


paddle : Paddle -> Position -> Svg Msg
paddle paddle position =
    S.rect
        [ A.class "paddle"
        , A.x <| toString <| getX position
        , A.y <| toString <| getY position
        , A.width <| toString <| getX paddle
        , A.height <| toString <| getY paddle
        ]
        []


view : Model -> Html Msg
view model =
    let
        width =
            toString <| M.width + 2 * M.paddleWidth

        height =
            toString <| M.height
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
