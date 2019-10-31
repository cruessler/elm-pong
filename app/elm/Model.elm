module Model exposing
    ( Backend(..)
    , Model
    , Position
    , ballHeight
    , ballWidth
    , halfBallHeight
    , halfBallWidth
    , height
    , paddleHeight
    , paddleWidth
    , width
    )

import Game exposing (Game, Paddle, Player(..))


type alias Position =
    { x : Int, y : Int }


type Backend
    = Svg


type alias Model =
    { game : Game
    , lastMousePosition : Maybe Position
    , backend : Backend
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
