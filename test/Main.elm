module Main (..) where

import ElmTest exposing (..)
import Console
import Task
import Collision2D as C2D
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
            [ C2D.vec2 1 1
            ]
        , testBad2dHull
            "Two vectors"
            [ C2D.vec2 1 1
            , C2D.vec2 2 2
            ]
        , testBad2dHull
            "Repeat vector"
            [ C2D.vec2 1 1
            , C2D.vec2 1 1
            , C2D.vec2 1 1
            ]
        , testBad2dHull
            "Clockwise path"
            [ C2D.vec2 0 2
            , C2D.vec2 2 2
            , C2D.vec2 1 1
            ]
        , testBad2dHull
            "path with concavity"
            [ C2D.vec2 0 0
            , C2D.vec2 2 -1
            , C2D.vec2 1 -1
            , C2D.vec2 1 1
            , C2D.vec2 2 1
            , C2D.vec2 0 0
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
            [ C2D.vec2 1 1
            , C2D.vec2 2 2
            , C2D.vec2 0 2
            ]
        , testGood2dHull
            "Triangle with double vertex"
            [ C2D.vec2 1 1
            , C2D.vec2 2 2
            , C2D.vec2 2 2
            , C2D.vec2 0 2
            ]
        , testGood2dHull
            "Triangle with extra constraints"
            [ C2D.vec2 0 0
            , C2D.vec2 2 2
            , C2D.vec2 0 2
            , C2D.vec2 2 0
            , C2D.vec2 3 3
            , C2D.vec2 -1 3
            ]
        ]


testBad2dHull : String -> List C2D.Vec2 -> Test
testBad2dHull name vectors =
    let
        hull =
            C2D.fromVectors vectors

        testOutside ( x, y ) =
            C2D.isOutside (C2D.vec2 x y) hull
                |> assert
                |> test (toString ( x, y ))
    in
        suite
            name
            [ testOutside ( 0, 0 )
            , testOutside ( 1, 1 )
            , testOutside ( 2, 2 )
            , testOutside ( 0, 2 )
            , testOutside ( 2, 0 )
            ]


testGood2dHull : String -> List C2D.Vec2 -> Test
testGood2dHull name vectors =
    let
        hull =
            C2D.fromVectors vectors

        testOutside ( x, y ) =
            C2D.isOutside (C2D.vec2 x y) hull
                |> assert
                |> test (toString ( x, y ) ++ " outside")

        testInside ( x, y ) =
            C2D.isInside (C2D.vec2 x y) hull
                |> assert
                |> test (toString ( x, y ) ++ " inside")
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
        testOutside ( x, y, z ) hull =
            test (toString ( x, y, z ))
                <| assert
                <| C3D.isOutside (C3D.vec3 x y z) hull

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
                    [ ( C3D.vec3 1 1 1, C3D.vec3 2 1 1, C3D.vec3 1 1 1 ) ]
                )
            ]


oneFace : Test
oneFace =
    let
        hull =
            C3D.fromTriangles
                [ ( C3D.vec3 0 0 0, C3D.vec3 1 1 0, C3D.vec3 0 1 0 ) ]

        testOutside name ( x, y, z ) =
            test name
                <| assert
                <| C3D.isOutside (C3D.vec3 x y z) hull

        testInside name ( x, y, z ) =
            test name
                <| assert
                <| C3D.isInside (C3D.vec3 x y z) hull
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
                [ ( C3D.vec3 0 0 0, C3D.vec3 1 1 0, C3D.vec3 0 1 0 )
                , ( C3D.vec3 0 1 0, C3D.vec3 1 1 0, C3D.vec3 0 1 -1 )
                ]

        testOutside name ( x, y, z ) =
            test name
                <| assert
                <| C3D.isOutside (C3D.vec3 x y z) hull

        testInside name ( x, y, z ) =
            test name
                <| assert
                <| C3D.isInside (C3D.vec3 x y z) hull
    in
        suite
            "Two-faced hull"
            [ testInside "at a vertex" ( 1, 1, 0 )
            , testInside "on an edge" ( 0.5, 1, 0 )
            , testInside "inside both faces" ( 0.5, 0.5, -0.5 )
            , testOutside "inside one face" ( 0.5, 0.5, 2 )
            , testOutside "outside both faces" ( 1, 3, -1 )
            ]



{-
Command line test harness
-}


port runner : Signal (Task.Task a ())
port runner =
    Console.run (ElmTest.consoleRunner allTests)
