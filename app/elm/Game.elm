module Game exposing
    ( Ball
    , Game
    , Opponent(..)
    , Paddle
    , Player
    , Position
    , advance
    , initialize
    , moveOpponent
    , movePaddle
    , movePlayerPaddle
    , reset
    )

import Math.Vector2 as V exposing (Vec2, getX, getY, setX, setY, vec2)


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
    Vec2


type alias Player =
    { position : Position
    , previousPosition : Position
    , score : Int
    }


type Opponent
    = One
    | Two


type alias Game =
    { board : Board
    , ball : Ball
    , position : Position
    , velocity : Maybe Velocity
    , paddle : Paddle
    , player1 : Player
    , player2 : Player
    , opponent : Opponent
    }


initialize : Ball -> Paddle -> Board -> Opponent -> Game
initialize ball paddle board opponent =
    let
        initialPosition1 =
            initialPlayerPosition 0 paddle board

        player1 =
            initialPlayer initialPosition1

        initialPosition2 =
            initialPlayerPosition (getX board + getX paddle) paddle board

        player2 =
            initialPlayer initialPosition2
    in
    { board = board
    , ball = ball
    , position = initialBallPosition board
    , velocity = Just <| vec2 0.0 0.0
    , paddle = paddle
    , player1 = player1
    , player2 = player2
    , opponent = opponent
    }


initialBallPosition : Board -> Position
initialBallPosition board =
    vec2 (getX board / 2) (getY board / 2)


initialPlayerPosition : Float -> Paddle -> Board -> Position
initialPlayerPosition x paddle board =
    vec2 x ((getY board - getY paddle) / 2)


initialPlayer : Position -> Player
initialPlayer initialPosition =
    { position = initialPosition
    , previousPosition = initialPosition
    , score = 0
    }


advance : Float -> Game -> Game
advance delta game =
    case game.velocity of
        Just velocity ->
            game
                |> move (V.scale delta velocity)
                |> moveOpponent game.opponent

        Nothing ->
            game


negateX : Vec2 -> Vec2
negateX v =
    setX -(getX v) v


negateY : Vec2 -> Vec2
negateY v =
    setY -(getY v) v


within : Float -> Float -> Float -> Bool
within a b c =
    a >= b && a <= c


hitsPaddle : Float -> Paddle -> Position -> Bool
hitsPaddle y paddle position =
    within
        y
        (getY position)
        (getY position + getY paddle)


reset : Game -> Game
reset game =
    let
        { paddle, board, player1, player2 } =
            game

        initialPosition1 =
            initialPlayerPosition 0 paddle board

        initialPosition2 =
            initialPlayerPosition (getX board + getX paddle) paddle board

        newPlayer1 =
            { player1 | position = initialPosition1, previousPosition = initialPosition1 }

        newPlayer2 =
            { player2 | position = initialPosition2, previousPosition = initialPosition2 }
    in
    { game
        | position = initialBallPosition game.board
        , velocity = Just <| vec2 150.0 0.0
        , player1 = newPlayer1
        , player2 = newPlayer2
    }


resetBall : Game -> Game
resetBall game =
    { game
        | position = initialBallPosition game.board
        , velocity = Nothing
    }


score : Opponent -> Game -> Game
score opponent game =
    let
        addOne : Player -> Player
        addOne player =
            { player | score = player.score + 1 }
    in
    case opponent of
        One ->
            { game | player1 = addOne game.player1 }

        Two ->
            { game | player2 = addOne game.player2 }


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

        halfBallWidth =
            getX game.ball / 2

        bounce : Float -> Game -> Game
        bounce yDiff game_ =
            let
                deflectedPath =
                    V.sub path pathToWall
                        |> negateX
            in
            move deflectedPath
                { game_
                    | velocity =
                        Maybe.map
                            (negateX >> accelerate yDiff)
                            game_.velocity
                    , position = newPosition
                }

        deflectBy : Position -> Position -> Float
        deflectBy previousPosition position =
            getY previousPosition - getY position

        f =
            let
                hitOrScore : Player -> Opponent -> (Game -> Game)
                hitOrScore player opponent =
                    if hitsPaddle (getY newPosition) game.paddle player.position then
                        bounce
                            (deflectBy
                                player.previousPosition
                                player.position
                            )

                    else
                        resetBall
                            >> score opponent
            in
            if getX newPosition <= halfBallWidth then
                hitOrScore game.player1 Two

            else if getX newPosition >= getX game.board - halfBallWidth then
                hitOrScore game.player2 One

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
            V.sub path pathToWall
                |> negateY
    in
    move deflectedPath
        { game
            | velocity = Maybe.map negateY game.velocity
            , position = V.add game.position pathToWall
        }


move : Vec2 -> Game -> Game
move path game =
    let
        newPosition =
            V.add game.position path

        halfBallWidth =
            getX game.ball / 2

        halfBallHeight =
            getY game.ball / 2

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
            [ ( getX newPosition < halfBallWidth
              , (getX game.position - halfBallWidth) / getX path |> abs
              , bounceX
              )
            , ( getX newPosition > (getX game.board - halfBallWidth)
              , (getX game.board - getX game.position - halfBallWidth) / getX path |> abs
              , bounceX
              )
            , ( getY newPosition < halfBallHeight
              , (getY game.position - halfBallHeight) / getY path |> abs
              , bounceY
              )
            , ( getY newPosition > (getY game.board - halfBallHeight)
              , (getY game.board - getY game.position - halfBallHeight) / getY path |> abs
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


movePlayerPaddle : Float -> Game -> Game
movePlayerPaddle yDiff game =
    case game.opponent of
        One ->
            movePaddle Two yDiff game

        Two ->
            movePaddle One yDiff game


movePaddle : Opponent -> Float -> Game -> Game
movePaddle opponent yDiff game =
    let
        movePaddle_ : Player -> Player
        movePaddle_ player =
            { player
                | position =
                    setY
                        (clamp
                            0
                            (getY game.board - getY game.paddle)
                            (getY player.position + yDiff)
                        )
                        player.position
                , previousPosition = player.position
            }
    in
    case opponent of
        One ->
            { game | player1 = movePaddle_ game.player1 }

        Two ->
            { game | player2 = movePaddle_ game.player2 }


moveOpponent : Opponent -> Game -> Game
moveOpponent opponent game =
    let
        moveBy =
            2.0

        paddleTop =
            case opponent of
                One ->
                    getY game.player1.position

                Two ->
                    getY game.player2.position

        paddleBottom =
            paddleTop
                + getY game.paddle

        ballY =
            getY game.position
    in
    if ballY < paddleTop then
        movePaddle opponent -moveBy game

    else if ballY > paddleBottom then
        movePaddle opponent moveBy game

    else
        game
