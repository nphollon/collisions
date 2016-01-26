module Main (..) where

import ElmTest exposing (..)
import Console
import Task
import Collision2D as C2D
import Math.Vector3 as Vec3 exposing (Vec3)
import Collision3D as C3D


allTests : Test
allTests =
    suite
        "All tests"
        [ suite
            "2D"
            [ empty2dHulls
            , triangleHulls
            ]
        , suite
            "3D"
            [ noFaces
            , oneFace
            , twoFaces
            ]
        ]



{-
Two Dimensions
-}


empty2dHulls : Test
empty2dHulls =
    suite
        "Vector lists that construct empty hulls"
        [ testBad2dHull
            "No vectors"
            []
        , testBad2dHull
            "One vector"
            [ vec2 1 1
            ]
        , testBad2dHull
            "Two vectors"
            [ vec2 1 1
            , vec2 2 2
            ]
        , testBad2dHull
            "Repeat vector"
            [ vec2 1 1
            , vec2 1 1
            , vec2 1 1
            ]
        , testBad2dHull
            "Clockwise path"
            [ vec2 0 2
            , vec2 2 2
            , vec2 1 1
            ]
        , testBad2dHull
            "path with concavity"
            [ vec2 0 0
            , vec2 2 -1
            , vec2 1 -1
            , vec2 1 1
            , vec2 2 1
            , vec2 0 0
            ]
        ]


triangleHulls : Test
triangleHulls =
    {-
    ^
    |
    +-----+
    |\.../
    | \./
    |  +
    |
    |
    +------>
    -}
    suite
        "Triangular hulls around (1,1) - (2,2) - (0,2)"
        [ testGood2dHull
            "Simple triangle"
            [ vec2 1 1
            , vec2 2 2
            , vec2 0 2
            ]
        , testGood2dHull
            "Triangle with double vertex"
            [ vec2 1 1
            , vec2 2 2
            , vec2 2 2
            , vec2 0 2
            ]
        , testGood2dHull
            "Triangle with extra constraints"
            [ vec2 0 0
            , vec2 2 2
            , vec2 0 2
            , vec2 2 0
            , vec2 3 3
            , vec2 -1 3
            ]
        ]


testBad2dHull : String -> List C2D.Vector -> Test
testBad2dHull name vectors =
    let
        hull =
            C2D.fromVectors vectors

        testOutside position =
            C2D.isOutside (vec2FromTuple position) hull
                |> assert
                |> test (toString position)
    in
        suite
            name
            [ testOutside ( 0, 0 )
            , testOutside ( 1, 1 )
            , testOutside ( 2, 2 )
            , testOutside ( 0, 2 )
            , testOutside ( 2, 0 )
            ]


testGood2dHull : String -> List C2D.Vector -> Test
testGood2dHull name vectors =
    let
        hull =
            C2D.fromVectors vectors

        testOutside position =
            C2D.isOutside (vec2FromTuple position) hull
                |> assert
                |> test (toString position ++ " outside")

        testInside position =
            C2D.isInside (vec2FromTuple position) hull
                |> assert
                |> test (toString position ++ " inside")
    in
        suite
            name
            [ testInside ( 2, 2 )
            , testInside ( 1, 2 )
            , testInside ( 1, 1.5 )
            , testOutside ( 0, 1 )
            , testOutside ( 1, 0 )
            , testOutside ( 2, 0 )
            ]



{-
Three Dimensions
-}


noFaces : Test
noFaces =
    let
        testOutside position hull =
            test (toString position)
                <| assert
                <| C3D.isOutside (vec3FromTuple position) hull

        testBad3dHull name hull =
            suite
                name
                [ testOutside ( 0, 0, 0 ) hull
                , testOutside ( 1, 1, 1 ) hull
                , testOutside ( 2, 2, 2 ) hull
                ]
    in
        suite
            "Triangles that construct empty hulls"
            [ testBad3dHull
                "No triangles"
                (C3D.fromTriangles [])
            , testBad3dHull
                "Triangle with repeat vertex"
                (C3D.fromTriangles
                    [ ( vec3 1 1 1, vec3 2 1 1, vec3 1 1 1 ) ]
                )
            ]


oneFace : Test
oneFace =
    let
        hull =
            C3D.fromTriangles
                [ ( vec3 0 0 0, vec3 1 1 0, vec3 0 1 0 ) ]

        testOutside name position =
            test name
                <| assert
                <| C3D.isOutside (vec3FromTuple position) hull

        testInside name position =
            test name
                <| assert
                <| C3D.isInside (vec3FromTuple position) hull
    in
        suite
            "One-faced hull"
            [ testInside "at a vertex" ( 1, 1, 0 )
            , testInside "on an edge" ( 0.5, 1, 0 )
            , testInside "inside triangle" ( 0.2, 0.5, 0 )
            , testInside "coplanar with triangle" ( -10, -2, 0 )
            , testOutside "above triangle" ( 0.2, 0.5, 1 )
            , testInside "below triangle" ( 0.2, 0.5, -1 )
            ]


twoFaces : Test
twoFaces =
    let
        hull =
            C3D.fromTriangles
                [ ( vec3 0 0 0, vec3 1 1 0, vec3 0 1 0 )
                , ( vec3 0 1 0, vec3 1 1 0, vec3 0 1 -1 )
                ]

        testOutside name position =
            test name
                <| assert
                <| C3D.isOutside (vec3FromTuple position) hull

        testInside name position =
            test name
                <| assert
                <| C3D.isInside (vec3FromTuple position) hull
    in
        suite
            "Two-faced hull"
            [ testInside "at a vertex" ( 1, 1, 0 )
            , testInside "on an edge" ( 0.5, 1, 0 )
            , testInside "inside both faces" ( 0.5, 0.5, -0.5 )
            , testOutside "inside one face" ( 0.5, 0.5, 2 )
            , testOutside "outside both faces" ( 1, 3, -1 )
            ]


vec2FromTuple : ( Float, Float ) -> C2D.Vector
vec2FromTuple ( x, y ) =
    { x = x, y = y }


vec2 : Float -> Float -> C2D.Vector
vec2 x y =
    { x = x, y = y }


vec3FromTuple : ( Float, Float, Float ) -> C3D.Vector
vec3FromTuple ( x, y, z ) =
    { x = x, y = y, z = z }


vec3 : Float -> Float -> Float -> C3D.Vector
vec3 x y z =
    { x = x, y = y, z = z }



{-
Command line test harness
-}


port runner : Signal (Task.Task a ())
port runner =
    Console.run (ElmTest.consoleRunner allTests)
