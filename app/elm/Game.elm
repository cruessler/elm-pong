module Game
    exposing
        ( Game
        , Paddle
        , Player(..)
        , initialize
        , advance
        , reset
        , movePaddle
        )

import Math.Vector2 as V exposing (Vec2, vec2, getX, getY, setX, setY)
import Time exposing (Time)
import Tuple exposing (first, second)


type alias Board =
    Vec2


type alias Ball =
    Vec2


type alias Position =
    Vec2


type alias Velocity =
    Vec2


{-| Represents a paddle’s y position and height.
-}
type alias Paddle =
    ( Float, Float )


type Player
    = One
    | Two


type alias Game =
    { board : Board
    , ball : Ball
    , position : Position
    , velocity : Velocity
    , player1 : Paddle
    , previousPlayer1 : Paddle
    , player2 : Paddle
    , previousPlayer2 : Paddle
    , lastTick : Time
    }


initialize : Ball -> Float -> Board -> Game
initialize ball paddleHeight board =
    let
        paddle1 =
            initialPaddlePosition paddleHeight board

        paddle2 =
            initialPaddlePosition paddleHeight board
    in
        { board = board
        , ball = ball
        , position = initialBallPosition board
        , velocity = vec2 0.0 0.0
        , player1 = paddle1
        , previousPlayer1 = paddle1
        , player2 = paddle2
        , previousPlayer2 = paddle2
        , lastTick = 0.0 * Time.second
        }


initialBallPosition : Board -> Position
initialBallPosition board =
    vec2 ((getX board) / 2) ((getY board) / 2)


initialPaddlePosition : Float -> Board -> Paddle
initialPaddlePosition height board =
    ( (getY board - height) / 2, height )


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


within : Float -> Float -> Float -> Bool
within a b c =
    a >= b && a <= c


hitsPaddle : Float -> Paddle -> Bool
hitsPaddle y paddle =
    within
        y
        (first paddle)
        (first paddle + second paddle)


reset : Time -> Game -> Game
reset lastTick game =
    { game
        | position = initialBallPosition game.board
        , velocity = vec2 150.0 0.0
        , previousPlayer1 = game.player1
        , previousPlayer2 = game.player2
        , lastTick = lastTick
    }


resetBall : Game -> Game
resetBall game =
    { game
        | position = initialBallPosition game.board
        , velocity = vec2 0.0 0.0
    }


accelerate : Float -> Vec2 -> Vec2
accelerate yDiff path =
    path
        |> V.add (vec2 5.0 (10.0 * -yDiff))


bounceX : Float -> Vec2 -> Game -> Game
bounceX r path game =
    let
        pathToWall =
            V.scale r path

        newPosition =
            V.add game.position pathToWall

        bounce : Float -> Game -> Game
        bounce yDiff game =
            let
                deflectedPath =
                    (V.sub path pathToWall)
                        |> negateX
            in
                move deflectedPath
                    { game
                        | velocity =
                            game.velocity
                                |> negateX
                                |> accelerate yDiff
                        , position = newPosition
                    }

        deflectBy : Paddle -> Paddle -> Float
        deflectBy ( y, _ ) paddle =
            y - first paddle

        f =
            if getX newPosition == 0 then
                if hitsPaddle (getY newPosition) game.player1 then
                    bounce
                        (deflectBy
                            game.previousPlayer1
                            game.player1
                        )
                else
                    resetBall
            else if getX newPosition == getX game.board then
                if hitsPaddle (getY newPosition) game.player2 then
                    bounce
                        (deflectBy
                            game.previousPlayer2
                            game.player2
                        )
                else
                    resetBall
            else
                bounce 0
    in
        f game


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


movePaddle : Player -> Float -> Game -> Game
movePaddle player yDiff game =
    case player of
        One ->
            { game
                | player1 =
                    Tuple.mapFirst
                        (\y -> clamp 0 (getY game.board - second game.player1) (y + yDiff))
                        game.player1
                , previousPlayer1 = game.player1
            }

        Two ->
            { game
                | player2 =
                    Tuple.mapFirst
                        (\y -> clamp 0 (getY game.board - second game.player2) (y + yDiff))
                        game.player2
                , previousPlayer2 = game.player2
            }
