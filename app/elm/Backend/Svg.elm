module Backend.Svg exposing (view)

import Game exposing (Game, Paddle)
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
        , A.cx <| toString <| getX game.position + M.paddleWidth + M.halfBallWidth
        , A.cy <| toString <| getY game.position + M.halfBallHeight
        , A.r <| toString <| M.halfBallWidth
        ]
        []


paddle : Float -> Paddle -> Svg Msg
paddle x ( y, height ) =
    S.rect
        [ A.class "paddle"
        , A.x <| toString x
        , A.y <| toString <| y + M.halfBallHeight
        , A.width <| toString M.paddleWidth
        , A.height <| toString height
        ]
        []


view : Model -> Html Msg
view model =
    let
        width =
            toString <| M.width + M.ballWidth + 2 * M.paddleWidth

        height =
            toString <| M.height + M.ballHeight
    in
        S.svg
            [ A.id "container"
            , A.viewBox <| "0 0 " ++ width ++ " " ++ height
            , E.onClick MouseClick
            ]
            [ ball model.game
            , paddle 0 model.game.player1
            , paddle (M.paddleWidth + getX model.game.board + M.ballWidth) model.game.player2
            ]
