module Tests exposing (..)

import Expect exposing (Expectation, FloatingPointTolerance(..))
import Fuzz exposing (Fuzzer, floatRange, int, list, string)
import Game exposing (Game, Player(..))
import Math.Vector2 as V exposing (Vec2, getX, getY, vec2)
import Test exposing (..)
import Time


gameWithFullHeightPaddles : Game
gameWithFullHeightPaddles =
    { board = vec2 10.0 10.0
    , ball = vec2 1.0 1.0
    , position = vec2 5.0 5.0
    , velocity = Just <| vec2 1.0 0.0
    , paddle = vec2 1.0 10.0
    , player1 = vec2 0.0 0.0
    , previousPlayer1 = vec2 0.0 0.0
    , player2 = vec2 10.0 0.0
    , previousPlayer2 = vec2 10.0 0.0
    , opponent = One
    }


gameWithoutPaddles : Game
gameWithoutPaddles =
    { gameWithFullHeightPaddles
        | paddle = vec2 0.0 0.0
    }


suite : Test
suite =
    describe "Game"
        [ describe "advance"
            [ test "ball moves horizontally" <|
                \_ ->
                    let
                        game =
                            gameWithFullHeightPaddles
                                |> Game.advance 1.0
                    in
                    Expect.all
                        [ getX >> Expect.within (Absolute 0.000001) 6.0
                        , getY >> Expect.within (Absolute 0.000001) 5.0
                        ]
                        game.position
            , test "ball moves vertically" <|
                \_ ->
                    let
                        game =
                            { gameWithFullHeightPaddles
                                | velocity = Just <| vec2 0.0 1.0
                            }
                                |> Game.advance 1.0
                    in
                    Expect.all
                        [ getX >> Expect.within (Absolute 0.000001) 5.0
                        , getY >> Expect.within (Absolute 0.000001) 6.0
                        ]
                        game.position
            , test "ball bounces off the right wall" <|
                \_ ->
                    let
                        game =
                            { gameWithFullHeightPaddles
                                | position = vec2 9.5 5.0
                            }
                                |> Game.advance 1.0
                    in
                    Expect.all
                        [ getX >> Expect.within (Absolute 0.000001) 8.5
                        , getY >> Expect.within (Absolute 0.000001) 5.0
                        ]
                        game.position
            , test "ball bounces off the left wall" <|
                \_ ->
                    let
                        game =
                            { gameWithFullHeightPaddles
                                | position = vec2 0.5 5.0
                                , velocity = Just <| vec2 -1.0 0.0
                            }
                                |> Game.advance 1.0
                    in
                    Expect.all
                        [ getX >> Expect.within (Absolute 0.000001) 1.5
                        , getY >> Expect.within (Absolute 0.000001) 5.0
                        ]
                        game.position
            , test "ball bounces off the top wall" <|
                \_ ->
                    let
                        game =
                            { gameWithFullHeightPaddles
                                | position = vec2 5.0 0.5
                                , velocity = Just <| vec2 0.0 -1.0
                            }
                                |> Game.advance 1.0
                    in
                    Expect.all
                        [ getX >> Expect.within (Absolute 0.000001) 5.0
                        , getY >> Expect.within (Absolute 0.000001) 1.5
                        ]
                        game.position
            , test "ball bounces off the bottom wall" <|
                \_ ->
                    let
                        game =
                            { gameWithFullHeightPaddles
                                | position = vec2 5.0 9.5
                                , velocity = Just <| vec2 0.0 1.0
                            }
                                |> Game.advance 1.0
                    in
                    Expect.all
                        [ getX >> Expect.within (Absolute 0.000001) 5.0
                        , getY >> Expect.within (Absolute 0.000001) 8.5
                        ]
                        game.position
            , test "ball bounces twice in top left corner" <|
                \_ ->
                    let
                        game =
                            { gameWithFullHeightPaddles
                                | position = vec2 1.0 2.0
                                , velocity = Just <| vec2 -3.0 -3.0
                            }
                                |> Game.advance 1.0
                    in
                    Expect.all
                        [ getX >> Expect.within (Absolute 0.000001) 3.0
                        , getY >> Expect.within (Absolute 0.000001) 2.0
                        ]
                        game.position
            , test "ball bounces twice in top right corner" <|
                \_ ->
                    let
                        game =
                            { gameWithFullHeightPaddles
                                | position = vec2 9.0 2.0
                                , velocity = Just <| vec2 3.0 -3.0
                            }
                                |> Game.advance 1.0
                    in
                    Expect.all
                        [ getX >> Expect.within (Absolute 0.000001) 7.0
                        , getY >> Expect.within (Absolute 0.000001) 2.0
                        ]
                        game.position
            , test "ball moves very slowly" <|
                \_ ->
                    let
                        game =
                            { gameWithFullHeightPaddles
                                | position = vec2 5.0 5.0
                                , velocity = Just <| vec2 0.000001 0.000001
                            }
                                |> Game.advance 1.0
                    in
                    Expect.all
                        [ getX >> Expect.within (Absolute 0.000001) 5.0
                        , getY >> Expect.within (Absolute 0.000001) 5.0
                        ]
                        game.position
            , test "ball moves very fast on diagonal" <|
                \_ ->
                    let
                        game =
                            { gameWithFullHeightPaddles
                                | position = vec2 5.0 5.0
                                , velocity = Just <| vec2 50.0 50.0
                            }
                                |> Game.advance 1.0
                    in
                    Expect.all
                        [ getX >> Expect.within (Absolute 0.000001) 1.0
                        , getY >> Expect.within (Absolute 0.000001) 1.0
                        ]
                        game.position
            , fuzz2
                (floatRange -50.0 50.0)
                (floatRange -50.0 50.0)
                "ball stays inside board"
              <|
                \x y ->
                    let
                        game =
                            { gameWithFullHeightPaddles
                                | velocity = Just <| vec2 x y
                            }
                                |> Game.advance 1.0
                    in
                    Expect.all
                        [ getX >> Expect.atLeast 0.0
                        , getX >> Expect.atMost (getX game.board)
                        , getY >> Expect.atLeast 0.0
                        , getY >> Expect.atMost (getY game.board)
                        ]
                        game.position
            , test "resets ball if paddle is missed" <|
                \_ ->
                    let
                        game =
                            { gameWithoutPaddles
                                | velocity = Just <| vec2 8.0 0.0
                            }
                                |> Game.advance 1.0
                    in
                    Expect.all
                        [ .position >> Expect.equal (vec2 5.0 5.0)
                        , .velocity >> Expect.equal Nothing
                        ]
                        game
            ]
        , describe "movePaddle"
            [ test "doesn’t leave the board with full height paddle" <|
                \_ ->
                    let
                        game =
                            gameWithFullHeightPaddles
                                |> Game.movePaddle One -1.0
                                |> Game.movePaddle Two 10.0
                                |> Game.movePaddle One 2.0
                                |> Game.movePaddle Two -5.0
                    in
                    Expect.all
                        [ .player1 >> V.toRecord >> Expect.equal { x = 0.0, y = 0.0 }
                        , .player2 >> V.toRecord >> Expect.equal { x = 10.0, y = 0.0 }
                        ]
                        game
            , test "doesn’t leave the board with 0-height paddle" <|
                \_ ->
                    let
                        game =
                            gameWithoutPaddles
                                |> Game.movePaddle One -1.0
                                |> Game.movePaddle Two 10.0
                                |> Game.movePaddle One 2.0
                                |> Game.movePaddle Two -6.0
                    in
                    Expect.all
                        [ .player1 >> V.toRecord >> Expect.equal { x = 0.0, y = 2.0 }
                        , .player2 >> V.toRecord >> Expect.equal { x = 10.0, y = 4.0 }
                        ]
                        game
            ]
        ]
