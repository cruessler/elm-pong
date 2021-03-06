module Main exposing (main)

import Backend.Svg
import Backend.WebGL
import Browser
import Browser.Events as Events
import Game exposing (Opponent(..))
import Html as H exposing (Html)
import Html.Attributes as A
import Html.Events as E
import Json.Decode as D
import Math.Vector2 exposing (vec2)
import Model as M exposing (Backend(..), Model, Position)
import Msg exposing (Msg(..))


main : Program () Model Msg
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
                One
      , lastMousePosition = Nothing
      , backend = Svg
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
                                Game.movePlayerPaddle
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

        SetBackend newBackend ->
            ( { model | backend = newBackend }, Cmd.none )


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


backendRadio : Backend -> Backend -> Html Msg
backendRadio newBackend currentBackend =
    let
        checked =
            newBackend == currentBackend
    in
    H.input
        [ A.type_ "radio"
        , A.checked checked
        , E.onClick (SetBackend newBackend)
        ]
        []


backendSwitch : Backend -> Html Msg
backendSwitch currentBackend =
    H.div [ A.id "backend-switch" ]
        [ H.h1 [] [ H.text "Choose backend" ]
        , H.label []
            [ backendRadio Svg currentBackend
            , H.text "SVG"
            ]
        , H.label []
            [ backendRadio WebGL currentBackend
            , H.text "WebGL"
            ]
        ]


scoreboard : Int -> Int -> Html Msg
scoreboard player1Score player2Score =
    H.div [ A.id "scoreboard" ]
        [ H.h1 [] [ H.text "Scores" ]
        , H.p []
            [ H.strong [] [ H.text "Your Opponent" ]
            , H.span [] [ H.text <| String.fromInt player1Score ]
            , H.strong [] [ H.text "You" ]
            , H.span [] [ H.text <| String.fromInt player2Score ]
            ]
        ]


backend : Model -> Html Msg
backend model =
    case model.backend of
        Svg ->
            Backend.Svg.view model

        WebGL ->
            Backend.WebGL.view model


view : Model -> Html Msg
view model =
    H.div [ A.id "viewport" ]
        [ backend model
        , H.div [ A.id "bottom-left-overlay" ]
            [ backendSwitch model.backend
            , scoreboard model.game.player1.score model.game.player2.score
            ]
        ]
