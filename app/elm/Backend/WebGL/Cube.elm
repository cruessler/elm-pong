module Backend.WebGL.Cube exposing (entity)

import Math.Matrix4 exposing (Mat4)
import Math.Vector3 as Vector3 exposing (Vec3, add, vec3)
import WebGL exposing (Entity, Mesh, Shader)


type alias Attribute =
    { aPosition : Vec3
    , aNormal : Vec3
    }


type alias Uniforms =
    { uColor : Vec3
    , uScale : Mat4
    , uPosition : Vec3
    , uPerspective : Mat4
    , uCamera : Mat4
    , uReverseLightDirection : Vec3
    }


type alias Varyings =
    { vLighting : Vec3 }


mesh : Mesh Attribute
mesh =
    let
        p1 =
            vec3 0 0 0

        p2 =
            vec3 1 0 0

        p3 =
            vec3 0 1 0

        p4 =
            vec3 1 1 0

        p5 =
            vec3 0 0 1

        p6 =
            vec3 1 0 1

        p7 =
            vec3 0 1 1

        p8 =
            vec3 1 1 1
    in
    [ face p1 p2 p3 p4
    , face p2 p6 p4 p8
    , face p3 p4 p7 p8
    , face p1 p2 p5 p6
    , face p1 p3 p5 p7
    ]
        |> List.concat
        |> WebGL.triangles


face : Vec3 -> Vec3 -> Vec3 -> Vec3 -> List ( Attribute, Attribute, Attribute )
face a b c d =
    let
        normal =
            Vector3.direction a b
                |> Vector3.cross (Vector3.direction b c)
    in
    [ ( Attribute a normal, Attribute b normal, Attribute c normal )
    , ( Attribute c normal, Attribute b normal, Attribute d normal )
    ]


vertexShader : Shader Attribute Uniforms Varyings
vertexShader =
    [glsl|
  attribute vec3 aPosition;
  attribute vec3 aNormal;

  uniform mat4 uScale;
  uniform vec3 uPosition;
  uniform vec3 uReverseLightDirection;

  uniform mat4 uPerspective;
  uniform mat4 uCamera;

  varying vec3 vLighting;

  void main () {
    gl_Position = uPerspective * uCamera * (uScale * vec4(aPosition, 1.0) + vec4(uPosition, 1.0));

    vec3 ambientLight = vec3(0.5, 0.5, 0.5);
    vec3 directionalLight = vec3(0.8, 0.8, 0.8);

    float light = max(dot(aNormal, uReverseLightDirection), 0.0);

    vLighting = ambientLight + (directionalLight * light);
  }
  |]


fragmentShader : Shader {} Uniforms Varyings
fragmentShader =
    [glsl|
  precision highp float;

  uniform vec3 uColor;

  varying vec3 vLighting;

  void main () {
    gl_FragColor = vec4(uColor * vLighting, 1.0);
  }
  |]


entity : Uniforms -> Entity
entity uniforms =
    WebGL.entity
        vertexShader
        fragmentShader
        mesh
        uniforms
