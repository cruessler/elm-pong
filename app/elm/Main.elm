module Main exposing (main)

import AnimationFrame
import Game exposing (Game, Paddle)
import Html as H exposing (Html)
import Html.Attributes as A
import Math.Vector2 exposing (vec2, getX, getY)
import Task
import Time exposing (Time)


type alias Model =
    { game : Game }


type Msg
    = Restart Time
    | Tick Time


main =
    H.program
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }


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


width : Float
width =
    400.0


height : Float
height =
    300.0


init : ( Model, Cmd Msg )
init =
    ( { game =
            Game.initialize
                (vec2 ballWidth ballHeight)
                paddleHeight
                (vec2 width height)
      }
    , Task.perform Restart Time.now
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Restart time ->
            ( { model | game = Game.reset time model.game }, Cmd.none )

        Tick time ->
            ( { model | game = Game.advance time model.game }, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    AnimationFrame.times Tick


ball : Game -> Html Msg
ball game =
    H.div
        [ A.id "ball"
        , A.style
            [ ( "border-width", (toString halfBallWidth) ++ "px" )
            , ( "border-radius", (toString halfBallWidth) ++ "px" )
            , ( "top", (getY game.position - halfBallHeight |> toString) ++ "px" )
            , ( "left", (getX game.position - halfBallWidth |> toString) ++ "px" )
            , ( "width", (toString ballWidth) ++ "px" )
            , ( "height", (toString ballHeight) ++ "px" )
            ]
        ]
        []


paddle : Float -> Paddle -> Html Msg
paddle x ( y, height ) =
    H.div
        [ A.class "paddle"
        , A.style
            [ ( "width", (toString paddleWidth) ++ "px" )
            , ( "height", (toString height) ++ "px" )
            , ( "top", (toString <| y + halfBallHeight) ++ "px" )
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
            , ( "top", (toString halfBallHeight) ++ "px" )
            , ( "left", (toString <| paddleWidth + halfBallWidth) ++ "px" )
            ]
        ]
        [ ball game
        ]


view : Model -> Html Msg
view model =
    H.div
        [ A.id "container"
        , A.style
            [ ( "width", (toString <| width + ballWidth + 2 * paddleWidth) ++ "px" )
            , ( "height", (toString <| height + ballHeight) ++ "px" )
            ]
        ]
        [ board model.game
        , paddle 0 model.game.player1
        , paddle (paddleWidth + getX model.game.board + ballWidth) model.game.player2
        ]
