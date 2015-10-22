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
                    [ emptyHulls
                    , triangleHull
                    ]
          , suite "3D"
                    [ fromTriangles
                    , noFaces
                    , oneFace
                    , twoFaces
                    ]
          ]


{-
Two Dimensions
-}

emptyHulls : Test
emptyHulls =
  suite "Vector lists that construct empty hulls"
          [ testBadHull "No vectors"
                          []
          , testBadHull "One vector"
                          [ Vec2.vec2 1 1
                          ]
          , testBadHull "Two vectors"
                          [ Vec2.vec2 1 1
                          , Vec2.vec2 2 2
                          ]
          , testBadHull "Repeat vector"
                          [ Vec2.vec2 1 1
                          , Vec2.vec2 1 1
                          , Vec2.vec2 1 1
                          ]
          , testBadHull "Clockwise path"
                          [ Vec2.vec2 0 2
                          , Vec2.vec2 2 2
                          , Vec2.vec2 1 1
                          ]
          , testBadHull "path with concavity"
                          [ Vec2.vec2 0 0
                          , Vec2.vec2 2 -1
                          , Vec2.vec2 1 -1
                          , Vec2.vec2 1 1
                          , Vec2.vec2 2 1
                          , Vec2.vec2 0 0
                          ]
          ]
                   
triangleHull : Test
triangleHull =
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
          [ testGoodHull "Simple triangle"
                           [ Vec2.vec2 1 1
                           , Vec2.vec2 2 2
                           , Vec2.vec2 0 2
                           ]
          , testGoodHull "Triangle with double vertex"
                           [ Vec2.vec2 1 1
                           , Vec2.vec2 2 2
                           , Vec2.vec2 2 2
                           , Vec2.vec2 0 2
                           ]
          , testGoodHull "Triangle with extra constraints"
                           [ Vec2.vec2 0 0
                           , Vec2.vec2 2 2
                           , Vec2.vec2 0 2
                           , Vec2.vec2 2 0
                           , Vec2.vec2 3 3
                           , Vec2.vec2 -1 3
                           ]
          ]


testBadHull : String -> List Vec2 -> Test
testBadHull name vectors =
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


testGoodHull : String -> List Vec2 -> Test
testGoodHull name vectors =
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

fromTriangles : Test
fromTriangles =
  let
    precision =
      1E-6
        
    triangles =
      [ (Vec3.vec3 0 0 0, Vec3.vec3 2 0 0, Vec3.vec3 0 1 0)
      , (Vec3.vec3 1 1 1, Vec3.vec3 1 2 1, Vec3.vec3 1 1 2)
      , (Vec3.vec3 -1 -2 -3, Vec3.vec3 -1 -2 -4, Vec3.vec3 0 -2 -3)
      ]

    linesAndTriangles =
      [ (Vec3.vec3 2 2 2, Vec3.vec3 2 2 2, Vec3.vec3 3 0 0)
      , (Vec3.vec3 0 0 0, Vec3.vec3 0 0 5, Vec3.vec3 0 0 9)
      ] ++ triangles

    expectedHull =
      [ { keyPoint = Vec3.vec3 0 0 0, normal = Vec3.vec3 0 0 1 }
      , { keyPoint = Vec3.vec3 1 1 1, normal = Vec3.vec3 1 0 0 }
      , { keyPoint = Vec3.vec3 -1 -2 -3, normal = Vec3.vec3 0 -1 0 }
      ]
  in
    suite "Making a 3D hull from a list of triangles"
            [ test "key point is first point, normal is cross product of face vectors"
                     <| compare3dHulls expectedHull (C3D.fromTriangles triangles)
                        
            , test "degenerate triangles do not get turned into faces"
                     <| compare3dHulls expectedHull (C3D.fromTriangles linesAndTriangles)
            ]


noFaces : Test
noFaces =
  test "Point is not inside a hull with no faces"
         <| assert
         <| C3D.isOutside [] (Vec3.vec3 0 0 0)


oneFace : Test
oneFace =
  suite "One-faced hull"
          [ test "Point in -Z is inside out-facing surface on Z=0"
                   <| assert
                   <| C3D.isInside [ face (0,0,0) (0,0,1) ] (Vec3.vec3 0 0 -5)

          , test "Point in +Z is inside in-facing surface on Z=0"
                   <| assert
                   <| C3D.isInside [ face (0,0,0) (0,0,-1) ] (Vec3.vec3 0 0 5)
          , test "Point in -Y is inside up-facing surface on Y=0"
                   <| assert
                   <| C3D.isInside [ face (0,0,0) (0,1,0) ] (Vec3.vec3 0 -5 5)
          , test "Point at X = 5 is outside left-facing surface on X = 10"
                   <| assert
                   <| C3D.isOutside [ face (10,0,0) (-1,0,0) ] (Vec3.vec3 5 0 0)
          , test "On the face counts as being inside"
                   <| assert
                   <| C3D.isInside [ face (1, 1, 1) (0.6, -0.8, 0) ] (Vec3.vec3 1.003 1.004 1)
          ]


twoFaces : Test
twoFaces =
  let
    hull =
      [ face (0, 0, 0) (0, 1, 0) 
      , face (0, 0, 0) (1, 0, 0)
      ]
  in
    suite "Two-faced hull"
            [ test "point is inside if inside both faces"
                     <| assert
                     <| C3D.isInside hull (Vec3.vec3 -10 -10 -10)
            , test "point is outside if outside both faces"
                     <| assert
                     <| C3D.isOutside hull (Vec3.vec3 10 10 -10)
            , test "point is outside if inside only one face"
                     <| assert
                     <| C3D.isOutside hull (Vec3.vec3 10 -10 -10)
            ]


compare3dHulls : C3D.Hull -> C3D.Hull -> Assertion
compare3dHulls expected actual =
  assertEqual (C3D.toPrintable expected) (C3D.toPrintable actual)


face : (Float, Float, Float) -> (Float, Float, Float)  -> C3D.Face
face keyPoint normal =
  { keyPoint = Vec3.fromTuple keyPoint
  , normal = Vec3.fromTuple normal
  }

{-
Command line test harness
-}

port requests : Signal IO.Runner.Request
port requests =
  IO.Runner.run responses (ConsoleRunner.runDisplay allTests)
    

port responses : Signal IO.Runner.Response
