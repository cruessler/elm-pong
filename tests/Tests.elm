module Tests exposing (..)

import Expect exposing (Expectation, FloatingPointTolerance(..))
import Fuzz exposing (Fuzzer, int, floatRange, list, string)
import Game exposing (Game)
import Math.Vector2 as V exposing (Vec2, vec2, getX, getY)
import Test exposing (..)
import Time


gameWithFullHeightPaddles : Game
gameWithFullHeightPaddles =
    { board = vec2 10.0 10.0
    , ball = vec2 1.0 1.0
    , position = vec2 5.0 5.0
    , velocity = vec2 1.0 0.0
    , player1 = ( 0.0, 10.0 )
    , player2 = ( 0.0, 10.0 )
    , lastTick = 0.0 * Time.second
    }


gameWithoutPaddles : Game
gameWithoutPaddles =
    { gameWithFullHeightPaddles
        | player1 = ( 0.0, 0.0 )
        , player2 = ( 0.0, 0.0 )
    }


game : Test
game =
    describe "Game"
        [ describe "advance"
            [ test "ball moves horizontally" <|
                \_ ->
                    let
                        game =
                            gameWithFullHeightPaddles
                                |> Game.advance (1.0 * Time.second)
                    in
                        Expect.all
                            [ getX >> Expect.equal 6.0
                            , getY >> Expect.equal 5.0
                            ]
                            game.position
            , test "ball moves vertically" <|
                \_ ->
                    let
                        game =
                            { gameWithFullHeightPaddles
                                | velocity = vec2 0.0 1.0
                            }
                                |> Game.advance (1.0 * Time.second)
                    in
                        Expect.all
                            [ getX >> Expect.equal 5.0
                            , getY >> Expect.equal 6.0
                            ]
                            game.position
            , test "ball bounces off the right wall" <|
                \_ ->
                    let
                        game =
                            { gameWithFullHeightPaddles
                                | position = vec2 9.5 5.0
                            }
                                |> Game.advance (1.0 * Time.second)
                    in
                        Expect.all
                            [ getX >> Expect.equal 9.5
                            , getY >> Expect.equal 5.0
                            ]
                            game.position
            , test "ball bounces off the left wall" <|
                \_ ->
                    let
                        game =
                            { gameWithFullHeightPaddles
                                | position = vec2 0.5 5.0
                                , velocity = vec2 -1.0 0.0
                            }
                                |> Game.advance (1.0 * Time.second)
                    in
                        Expect.all
                            [ getX >> Expect.equal 0.5
                            , getY >> Expect.equal 5.0
                            ]
                            game.position
            , test "ball bounces off the top wall" <|
                \_ ->
                    let
                        game =
                            { gameWithFullHeightPaddles
                                | position = vec2 5.0 0.5
                                , velocity = vec2 0.0 -1.0
                            }
                                |> Game.advance (1.0 * Time.second)
                    in
                        Expect.all
                            [ getX >> Expect.equal 5.0
                            , getY >> Expect.equal 0.5
                            ]
                            game.position
            , test "ball bounces off the bottom wall" <|
                \_ ->
                    let
                        game =
                            { gameWithFullHeightPaddles
                                | position = vec2 5.0 9.5
                                , velocity = vec2 0.0 1.0
                            }
                                |> Game.advance (1.0 * Time.second)
                    in
                        Expect.all
                            [ getX >> Expect.equal 5.0
                            , getY >> Expect.equal 9.5
                            ]
                            game.position
            , test "ball bounces twice in top left corner" <|
                \_ ->
                    let
                        game =
                            { gameWithFullHeightPaddles
                                | position = vec2 1.0 2.0
                                , velocity = vec2 -3.0 -3.0
                            }
                                |> Game.advance (1.0 * Time.second)
                    in
                        Expect.all
                            [ getX >> Expect.equal 2.0
                            , getY >> Expect.equal 1.0
                            ]
                            game.position
            , test "ball bounces twice in top right corner" <|
                \_ ->
                    let
                        game =
                            { gameWithFullHeightPaddles
                                | position = vec2 9.0 2.0
                                , velocity = vec2 3.0 -3.0
                            }
                                |> Game.advance (1.0 * Time.second)
                    in
                        Expect.all
                            [ getX >> Expect.equal 8.0
                            , getY >> Expect.equal 1.0
                            ]
                            game.position
            , test "ball moves very slowly" <|
                \_ ->
                    let
                        game =
                            { gameWithFullHeightPaddles
                                | position = vec2 5.0 5.0
                                , velocity = vec2 0.000001 0.000001
                            }
                                |> Game.advance (1.0 * Time.second)
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
                                , velocity = vec2 50.0 50.0
                            }
                                |> Game.advance (1.0 * Time.second)
                    in
                        Expect.all
                            [ getX >> Expect.within (Absolute 0.000001) 5.0
                            , getY >> Expect.within (Absolute 0.000001) 5.0
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
                                | velocity = vec2 x y
                            }
                                |> Game.advance (1.0 * Time.second)
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
                                | velocity = vec2 8.0 0.0
                            }
                                |> Game.advance (1.0 * Time.second)
                    in
                        Expect.all
                            [ .position >> Expect.equal (vec2 5.0 5.0)
                            , .velocity >> Expect.equal (vec2 0.0 0.0)
                            ]
                            game
            ]
        ]
