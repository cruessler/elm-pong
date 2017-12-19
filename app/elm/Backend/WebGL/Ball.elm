module Backend.WebGL.Ball exposing (entity)

import Math.Matrix4 exposing (Mat4)
import Math.Vector3 exposing (Vec3, add, normalize, vec3)
import WebGL exposing (Entity, Mesh, Shader)



{- The functions in this file come from [Planet3D], and are only slightly
   modified.

   `mesh` creates a sphere by taking an [octahedron], a solid composed of 8
   equilateral triangles, and recursively dividing its triangles into smaller
   triangles while maintaining the invariant that each vertex has the same
   distance to the origin of the coordinate system. Once the triangles are
   sufficiently small, the resulting solid looks like a sphere.

   [octahedron]: https://en.wikipedia.org/wiki/Octahedron
   [Planet3D]: https://github.com/w0rm/elm-webgl-playground/blob/master/Planet3D.elm
-}


type alias Attribute =
    { aPosition : Vec3
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


octahedron : List ( Vec3, Vec3, Vec3 )
octahedron =
    let
        p1 =
            vec3 0 1 0

        p2 =
            vec3 0 0 1

        p3 =
            vec3 1 0 0

        p4 =
            vec3 0 0 -1

        p5 =
            vec3 -1 0 0

        p6 =
            vec3 0 -1 0
    in
    [ ( p1, p2, p3 )
    , ( p1, p3, p4 )
    , ( p1, p4, p5 )
    , ( p1, p5, p2 )
    , ( p6, p2, p3 )
    , ( p6, p3, p4 )
    , ( p6, p4, p5 )
    , ( p6, p5, p2 )
    ]


divideSphere : Int -> List ( Vec3, Vec3, Vec3 ) -> List ( Vec3, Vec3, Vec3 )
divideSphere step triangles =
    if step <= 0 then
        triangles

    else
        divideSphere (step - 1) (List.concatMap divide triangles)


divide : ( Vec3, Vec3, Vec3 ) -> List ( Vec3, Vec3, Vec3 )
divide ( v0, v1, v2 ) =
    let
        a =
            add v0 v2 |> normalize

        b =
            add v0 v1 |> normalize

        c =
            add v1 v2 |> normalize
    in
    [ ( v0, b, a ), ( b, v1, c ), ( a, b, c ), ( a, c, v2 ) ]


mesh : Mesh Attribute
mesh =
    divideSphere 3 octahedron
        |> List.map
            (\( a, b, c ) ->
                ( Attribute a
                , Attribute b
                , Attribute c
                )
            )
        |> WebGL.triangles


vertexShader : Shader Attribute Uniforms Varyings
vertexShader =
    [glsl|
  attribute vec3 aPosition;

  uniform mat4 uScale;
  uniform vec3 uPosition;
  uniform vec3 uReverseLightDirection;

  uniform mat4 uPerspective;
  uniform mat4 uCamera;

  varying vec3 vLighting;

  void main () {
    gl_Position = uPerspective * uCamera * (uScale * vec4(aPosition, 1.0) + vec4(uPosition, 1.0));

    vec3 ambientLight = vec3(0.2, 0.2, 0.2);
    vec3 directionalLight = vec3(0.9, 0.9, 0.9);

    float light = max(dot(aPosition, uReverseLightDirection), 0.0);

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
