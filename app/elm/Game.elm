module Game exposing (..)

import Math.Vector2 as V exposing (Vec2, vec2, getX, getY, setX, setY)
import Time exposing (Time)


type alias Board =
    Vec2


type alias Ball =
    Vec2


type alias Position =
    Vec2


type alias Velocity =
    Vec2


type alias Game =
    { board : Board
    , ball : Ball
    , position : Position
    , velocity : Velocity
    , lastTick : Time
    }


advance : Float -> Game -> Game
advance to game =
    let
        timeDiff =
            (to - game.lastTick) |> Time.inSeconds

        newGame =
            game
                |> move (V.scale timeDiff game.velocity)
    in
        { newGame | lastTick = to }


negateX : Vec2 -> Vec2
negateX v =
    setX -(getX v) v


negateY : Vec2 -> Vec2
negateY v =
    setY -(getY v) v


bounceX : Float -> Vec2 -> Game -> Game
bounceX r path game =
    let
        pathToWall =
            V.scale r path

        deflectedPath =
            (V.sub path pathToWall)
                |> negateX
    in
        move deflectedPath
            { game
                | velocity = negateX game.velocity
                , position = (V.add game.position pathToWall)
            }


bounceY : Float -> Vec2 -> Game -> Game
bounceY r path game =
    let
        pathToWall =
            V.scale r path

        deflectedPath =
            (V.sub path pathToWall)
                |> negateY
    in
        move deflectedPath
            { game
                | velocity = negateY game.velocity
                , position = (V.add game.position pathToWall)
            }


move : Vec2 -> Game -> Game
move path game =
    let
        newPosition =
            V.add game.position path

        {- This list initially contains 4 3-tuples for each side of the board
           that can be hit by the ball, in the order: left, right, top, bottom.

           The tuple’s first element is a `Bool` indicating whether the
           respective side gets hit. The second element is a `Float` indicating
           which ratio of the ball’s path lies inside the board in case it hits
           a side. The third element is the function to call in case of a
           collision.

           This list is filtered to only contain tuples that represent an
           actual collision; it is then sorted by distance to the point of
           collision so that the closest collision takes precedence.
        -}
        bounces =
            [ ( getX newPosition < 0
              , (getX game.position) / getX path |> abs
              , bounceX
              )
            , ( getX newPosition > getX game.board
              , (getX game.board - getX game.position) / getX path |> abs
              , bounceX
              )
            , ( getY newPosition < 0
              , (getY game.position) / getY path |> abs
              , bounceY
              )
            , ( getY newPosition > getY game.board
              , (getY game.board - getY game.position) / getY path |> abs
              , bounceY
              )
            ]
                |> List.filter (\( b, _, _ ) -> b)
                |> List.sortBy (\( _, r, _ ) -> r)
    in
        bounces
            |> List.head
            |> Maybe.map (\( _, r, f ) -> f r path game)
            |> Maybe.withDefault
                { game
                    | position =
                        newPosition
                }
