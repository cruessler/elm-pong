module Main exposing (main)

import AnimationFrame
import Backend.Svg
import Game exposing (Player(..))
import Html as H exposing (Html)
import Math.Vector2 exposing (vec2)
import Model as M exposing (Model)
import Msg exposing (Msg(..))
import Mouse
import Task
import Time


main =
    H.program
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }


init : ( Model, Cmd Msg )
init =
    ( { game =
            Game.initialize
                (vec2 M.ballWidth M.ballHeight)
                (vec2 M.paddleWidth M.paddleHeight)
                (vec2 M.width M.height)
      , lastMousePosition = Nothing
      }
    , Task.perform Restart Time.now
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Restart time ->
            ( { model
                | lastMousePosition = Nothing
                , game = Game.reset time model.game
              }
            , Cmd.none
            )

        Tick time ->
            ( { model | game = Game.advance time model.game }, Cmd.none )

        MouseMove position ->
            let
                newGame =
                    model.lastMousePosition
                        |> Maybe.map
                            (\lastPosition ->
                                Game.movePaddle Two
                                    (toFloat <| position.y - lastPosition.y)
                                    model.game
                            )
                        |> Maybe.withDefault model.game
            in
                ( { model
                    | lastMousePosition = Just position
                    , game = newGame
                  }
                , Cmd.none
                )

        MouseClick ->
            ( model, Task.perform Restart Time.now )


subscriptions : Model -> Sub Msg
subscriptions model =
    let
        tuple =
            model.game.velocity
                |> Math.Vector2.toTuple
    in
        case tuple of
            ( 0.0, 0.0 ) ->
                Sub.none

            _ ->
                Sub.batch
                    [ AnimationFrame.times Tick
                    , Mouse.moves MouseMove
                    ]


view : Model -> Html Msg
view model =
    Backend.Svg.view model
