module Backend.WebGL exposing (view)

import Backend.WebGL.Ball as Ball
import Backend.WebGL.Cube as Cube
import Game exposing (Position)
import Html as H exposing (Html)
import Html.Attributes as A
import Html.Events as E
import Math.Matrix4 as Matrix4 exposing (Mat4)
import Math.Vector2 as Vector2 exposing (Vec2, vec2)
import Math.Vector3 exposing (Vec3, getX, getY, getZ, setX, setY, setZ, vec3)
import Model exposing (Model)
import Msg exposing (Msg(..))
import WebGL exposing (Entity, Shader)


makePerspective : Float -> Float -> Mat4
makePerspective width height =
    Matrix4.makePerspective 45 (width / height) 0.01 600


makeCamera : Float -> Float -> Mat4
makeCamera width height =
    let
        w2 =
            width / 4

        h2 =
            height / 4
    in
    Matrix4.makeLookAt (vec3 w2 h2 -250) (vec3 w2 h2 0) (vec3 0 -1 0)


setZ : Float -> Vec2 -> Vec3
setZ z v =
    vec3 (Vector2.getX v) (Vector2.getY v) z


addX : Float -> Vec2 -> Vec2
addX x v =
    vec2 (Vector2.getX v + x) (Vector2.getY v)


view : Model -> Html Msg
view model =
    let
        width =
            Vector2.getX model.game.board + 2 * Vector2.getX model.game.paddle

        height =
            Vector2.getY model.game.board

        scaleRatio =
            3

        ballPosition =
            model.game.position
                |> addX (Vector2.getX model.game.paddle)
                |> setZ (Vector2.getX model.game.ball / 2)

        paddle1 =
            setZ 0 model.game.player1

        paddle2 =
            setZ 0 model.game.player2

        paddleHeight =
            Vector2.getX model.game.paddle

        paddleScale =
            Matrix4.makeScale (setZ paddleHeight model.game.paddle)

        r =
            Vector2.getX model.game.ball / 2

        wallHeight =
            30

        -- Uniforms for the ball.
        uniforms =
            { uColor = vec3 0.4 0.0 0.0
            , uPosition = ballPosition
            , uPerspective = makePerspective width height
            , uCamera = makeCamera width height
            , uReverseLightDirection = vec3 0 0 -1
            , uScale = Matrix4.makeScale (vec3 r r r)
            }
    in
    WebGL.toHtml
        [ A.id "container"
        , A.class "webgl"

        -- Width and height in CSS pixels (the more, the better the canvas
        -- scales to bigger sizes).
        , A.width (width * scaleRatio |> round)
        , A.height (height * scaleRatio |> round)
        , E.onClick MouseClick
        ]
        [ Cube.entity
            { uniforms
                | uScale = paddleScale
                , uPosition = paddle1
            }
        , Cube.entity
            { uniforms
                | uScale = paddleScale
                , uPosition = paddle2
            }
        , Ball.entity
            uniforms

        -- The lower wall.
        , Cube.entity
            { uniforms
                | uScale = Matrix4.makeScale (vec3 width wallHeight paddleHeight)
                , uPosition = vec3 0 height 0
            }

        -- The upper wall.
        , Cube.entity
            { uniforms
                | uScale = Matrix4.makeScale (vec3 width wallHeight paddleHeight)
                , uPosition = vec3 0 -wallHeight 0
            }
        ]
