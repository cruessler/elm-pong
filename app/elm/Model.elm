module Model
    exposing
        ( Model
        , width
        , height
        , ballWidth
        , halfBallWidth
        , ballHeight
        , halfBallHeight
        , paddleWidth
        , paddleHeight
        )

import Game exposing (Game, Paddle, Player(..))
import Mouse exposing (Position)


type alias Model =
    { game : Game
    , lastMousePosition : Maybe Position
    }


width : Float
width =
    400.0


height : Float
height =
    300.0


ballWidth : Float
ballWidth =
    30.0


halfBallWidth : Float
halfBallWidth =
    ballWidth / 2


ballHeight : Float
ballHeight =
    ballWidth


halfBallHeight : Float
halfBallHeight =
    ballHeight / 2


paddleWidth : Float
paddleWidth =
    20.0


paddleHeight : Float
paddleHeight =
    100.0
