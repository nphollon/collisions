module Main where

import ElmTest.Test as Test exposing (Test, test, suite)
import ElmTest.Assertion exposing (Assertion, assert, assertEqual)
import ElmTest.Runner.Element as GraphicsRunner
import ElmTest.Runner.Console as ConsoleRunner
import IO.Runner

import Math.Vector2 as Vec2 exposing (Vec2)
import Collision2D as C2D

import Math.Vector3 as Vec3 exposing (Vec3)
import Collision3D as C3D


allTests : Test
allTests =
  suite "All tests"
          [ suite "2D"
                    [ empty2dHulls
                    , triangleHulls
                    ]
          , suite "3D"
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
  suite "Vector lists that construct empty hulls"
          [ testBad2dHull "No vectors"
                            []
          , testBad2dHull "One vector"
                            [ Vec2.vec2 1 1
                            ]
          , testBad2dHull "Two vectors"
                            [ Vec2.vec2 1 1
                            , Vec2.vec2 2 2
                            ]
          , testBad2dHull "Repeat vector"
                            [ Vec2.vec2 1 1
                            , Vec2.vec2 1 1
                            , Vec2.vec2 1 1
                            ]
          , testBad2dHull "Clockwise path"
                            [ Vec2.vec2 0 2
                            , Vec2.vec2 2 2
                            , Vec2.vec2 1 1
                            ]
          , testBad2dHull "path with concavity"
                            [ Vec2.vec2 0 0
                            , Vec2.vec2 2 -1
                            , Vec2.vec2 1 -1
                            , Vec2.vec2 1 1
                            , Vec2.vec2 2 1
                            , Vec2.vec2 0 0
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
  suite "Triangular hulls around (1,1) - (2,2) - (0,2)"
          [ testGood2dHull "Simple triangle"
                             [ Vec2.vec2 1 1
                             , Vec2.vec2 2 2
                             , Vec2.vec2 0 2
                             ]
          , testGood2dHull "Triangle with double vertex"
                             [ Vec2.vec2 1 1
                             , Vec2.vec2 2 2
                             , Vec2.vec2 2 2
                             , Vec2.vec2 0 2
                             ]
          , testGood2dHull "Triangle with extra constraints"
                             [ Vec2.vec2 0 0
                             , Vec2.vec2 2 2
                             , Vec2.vec2 0 2
                             , Vec2.vec2 2 0
                             , Vec2.vec2 3 3
                             , Vec2.vec2 -1 3
                             ]
          ]


testBad2dHull : String -> List Vec2 -> Test
testBad2dHull name vectors =
  let
    hull =
      C2D.fromVectors vectors

    testOutside position =
      C2D.isOutside (Vec2.fromTuple position) hull
        |> assert
        |> test (toString position)
  in
    suite name
            [ testOutside (0, 0)
            , testOutside (1, 1)
            , testOutside (2, 2)
            , testOutside (0, 2)
            , testOutside (2, 0)
            ]


testGood2dHull : String -> List Vec2 -> Test
testGood2dHull name vectors =
  let
    hull =
      C2D.fromVectors vectors

    testOutside position =
      C2D.isOutside (Vec2.fromTuple position) hull
        |> assert
        |> test (toString position ++ " outside")

    testInside position =
      C2D.isInside (Vec2.fromTuple position) hull
        |> assert
        |> test (toString position ++ " inside")
  in
    suite name
            [ testInside (2, 2)
            , testInside (1, 2)
            , testInside (1, 1.5)
            , testOutside (0, 1)
            , testOutside (1, 0)
            , testOutside (2, 0)
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
             <| C3D.isOutside (Vec3.fromTuple position) hull
                
    testBad3dHull name hull =
      suite name
            [ testOutside (0, 0, 0) hull
            , testOutside (1, 1, 1) hull
            , testOutside (2, 2, 2) hull
            ]
  in
    suite "Triangles that construct empty hulls"
            [ testBad3dHull "No triangles"
                              (C3D.fromTriangles [])
            , testBad3dHull "Triangle with repeat vertex"
                              (C3D.fromTriangles
                                    [ (Vec3.vec3 1 1 1, Vec3.vec3 2 1 1, Vec3.vec3 1 1 1) ]
                              )
            ]


oneFace : Test
oneFace =
  let
    hull =
      C3D.fromTriangles
           [ (Vec3.vec3 0 0 0, Vec3.vec3 1 1 0, Vec3.vec3 0 1 0) ]

    testOutside name position =
      test name
             <| assert
             <| C3D.isOutside (Vec3.fromTuple position) hull

    testInside name position =
      test name
             <| assert
             <| C3D.isInside (Vec3.fromTuple position) hull
  in
    suite "One-faced hull"
          [ testInside "at a vertex" (1, 1, 0)
          , testInside "on an edge" (0.5, 1, 0)
          , testInside "inside triangle" (0.2, 0.5, 0)
          , testInside "coplanar with triangle" (-10, -2, 0)
          , testOutside "above triangle" (0.2, 0.5, 1)
          , testInside "below triangle" (0.2, 0.5, -1)
          ]


twoFaces : Test
twoFaces =
  let
    hull =
      C3D.fromTriangles
           [ (Vec3.vec3 0 0 0, Vec3.vec3 1 1 0, Vec3.vec3 0 1 0)
           , (Vec3.vec3 0 1 0, Vec3.vec3 1 1 0, Vec3.vec3 0 1 -1)
           ]

    testOutside name position =
      test name
             <| assert
             <| C3D.isOutside (Vec3.fromTuple position) hull

    testInside name position =
      test name
             <| assert
             <| C3D.isInside (Vec3.fromTuple position) hull
  in
    suite "Two-faced hull"
            [ testInside "at a vertex" (1, 1, 0)
            , testInside "on an edge" (0.5, 1, 0)
            , testInside "inside both faces" (0.5, 0.5, -0.5)
            , testOutside "inside one face" (0.5, 0.5, 2)
            , testOutside "outside both faces" (1, 3, -1)
            ]

{-
Command line test harness
-}

port requests : Signal IO.Runner.Request
port requests =
  IO.Runner.run responses (ConsoleRunner.runDisplay allTests)
    

port responses : Signal IO.Runner.Response
