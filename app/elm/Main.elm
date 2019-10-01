module Main exposing (main)

import Backend.Svg
import Browser
import Browser.Events as Events
import Game exposing (Player(..))
import Html as H exposing (Html)
import Json.Decode as D
import Math.Vector2 exposing (vec2)
import Model as M exposing (Model, Position)
import Msg exposing (Msg(..))
import Task
import Time


main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { game =
            Game.initialize
                (vec2 M.ballWidth M.ballHeight)
                (vec2 M.paddleWidth M.paddleHeight)
                (vec2 M.width M.height)
      , lastMousePosition = Nothing
      }
    , Cmd.none
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick delta ->
            ( { model | game = Game.advance (delta / 1000) model.game }, Cmd.none )

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
            ( { model
                | lastMousePosition = Nothing
                , game = Game.reset model.game
              }
            , Cmd.none
            )


subscriptions : Model -> Sub Msg
subscriptions model =
    case model.game.velocity of
        Nothing ->
            Sub.none

        _ ->
            let
                decodePosition : D.Decoder Position
                decodePosition =
                    D.map2 Position (D.at [ "offsetX" ] D.int) (D.at [ "offsetY" ] D.int)
            in
            Sub.batch
                [ Events.onAnimationFrameDelta Tick
                , Events.onMouseMove (D.map MouseMove decodePosition)
                ]


view : Model -> Html Msg
view model =
    Backend.Svg.view model
