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
          [ suite "2D collision"
                    [ noSides
                    , oneSide
                    , twoSidesOneHull
                    , twoSidesTwoHulls
                    ]
          , suite "Hull construction"
                    [ fromSegments
                    , fromVertexes
                    , fromTriangles
                    ]
          , suite "3D collision"
                    [ noFaces
                    , oneFace
                    , twoFacesOneHull
                    , twoFacesTwoHulls
                    ]
          ]


{-
Two Dimensions
-}

noSides : Test
noSides =
  suite "Empty boundary"
          [ test "Point is not inside a hull with no sides"
                   <| assert
                   <| C2D.isOutside [[]] (Vec2.vec2 0 0)
                      
          , test "Point is not inside a boundary with no hulls"
                   <| assert
                   <| C2D.isOutside [] (Vec2.vec2 0 0)                  
          ]
  
oneSide : Test
oneSide =
  suite "One-sided hull"
          [ test "Point in -Y is inside up-facing side on X-axis"
                   <| assert
                   <| C2D.isInside [[ floorAt (Vec2.vec2 0 0) ]] (Vec2.vec2 0 -3)
                           
          , test "Point in -Y is outside up-facing side below X-axis"
                   <| assert
                   <| C2D.isOutside [[ floorAt (Vec2.vec2 0 -10) ]] (Vec2.vec2 0 -3)

          , test "On the line counts as being inside"
                   <| assert
                   <| C2D.isInside [[ floorAt (Vec2.vec2 0 0) ]] (Vec2.vec2 0 0)
                           
          , test "Point in +Y is inside down-facing side on X-axis"
                   <| assert
                   <| C2D.isInside [[ ceilingAt (Vec2.vec2 0 0) ]] (Vec2.vec2 0 3)
                           
          , test "Point in quadrant II is outside up-facing side on X=Y"
                   <| assert
                   <| C2D.isOutside [[ slopeAt (Vec2.vec2 10 10) ]] (Vec2.vec2 -1 2)
          ]
  

twoSidesOneHull : Test
twoSidesOneHull =
  let
    {-
         /
        /    
    ---+---
      /....
     /.....

     -}
    
    hull =
      [ [ floorAt (Vec2.vec2 0 0)
        , slopeAt (Vec2.vec2 0 0)
        ]
      ]
  in
    suite "Two-sided hull"
            [ test "Point is inside hull if inside both sides"
                     <| assert
                     <| C2D.isInside hull (Vec2.vec2 2 -1)

            , test "Point is outside hull if outside both sides"
                     <| assert
                     <| C2D.isOutside hull (Vec2.vec2 -2 1)

            , test "Point is outside hull if inside only one side"
                     <| assert
                     <| C2D.isOutside hull (Vec2.vec2 -2 -1)
                        
            , test "Point is inside if on both sides"
                     <| assert
                     <| C2D.isInside
                          [ C2D.fromVertexes
                                 (List.map Vec2.fromTuple [ (-90, 30), (-60, 50), (-70, 0) ])
                          ]
                          (Vec2.vec2 -90 30)
            ]


twoSidesTwoHulls : Test
twoSidesTwoHulls =
  let
    {-
         /.
        /..    
    ---+---
    ../....
    ./.....

     -}
    
    hulls =
      [ [ floorAt (Vec2.vec2 0 0) ]
      , [ slopeAt (Vec2.vec2 0 0) ]
      ]
  in
    suite "Two one-sided hulls"
            [ test "Point is inside boundary if inside both hulls"
                     <| assert
                     <| C2D.isInside hulls (Vec2.vec2 2 -1)

            , test "Point is outside boundary if outside both hulls"
                     <| assert
                     <| C2D.isOutside hulls (Vec2.vec2 -2 1)

            , test "Point is inside boundary if inside only one hull"
                     <| assert
                     <| C2D.isInside hulls (Vec2.vec2 -2 -1)
            ]


fromSegments : Test
fromSegments =
  let
    segments =
      [ (Vec2.vec2 0 0, Vec2.vec2 0 2)
      , (Vec2.vec2 0 1, Vec2.vec2 1 1)
      ]

    nullSegments =
      [ (Vec2.vec2 3 1, Vec2.vec2 3 1)
      ]

    expectedHull =
      [ { keyPoint = Vec2.vec2 0 0, normal = Vec2.vec2 -1 0 }
      , { keyPoint = Vec2.vec2 0 1, normal = Vec2.vec2 0 1 }
      ]
  in
    suite "Making a hull from a list of line segments"
            [ test "First point is key point, normal directed anti-clockwise"
                     <| compare2dHulls expectedHull (C2D.fromSegments segments)
                        
            , test "Line segment length 0 does not return a side"
                     <| compare2dHulls [] (C2D.fromSegments nullSegments)
            ]


fromVertexes : Test
fromVertexes =
  let
    shortList =
      [ Vec2.vec2 0 0
      , Vec2.vec2 1 1
      ]

    triangleVertexes =
      [ Vec2.vec2 0 0
      , Vec2.vec2 4 3
      , Vec2.vec2 4 0
      ]

    triangleSides =
      [ { keyPoint = Vec2.vec2 0 0, normal = Vec2.vec2 -0.6 0.8 }
      , { keyPoint = Vec2.vec2 4 3, normal = Vec2.vec2 1 0 }
      , { keyPoint = Vec2.vec2 4 0, normal = Vec2.vec2 0 -1 }
      ]

    doubleVertexes =
      [ Vec2.vec2 4 3
      , Vec2.vec2 4 5
      , Vec2.vec2 4 3
      ]

    doubleSides =
      [ { keyPoint = Vec2.vec2 4 3, normal = Vec2.vec2 -1 0 }
      , { keyPoint = Vec2.vec2 4 5, normal = Vec2.vec2 1 0 }
      ]
  in
    suite "Making a hull from a list of points"
            [ test "Two or fewer points returns an empty hull"
                     <| compare2dHulls [] (C2D.fromVertexes shortList)

            , test "Three or more points returns sides connecting all in series"
                     <| compare2dHulls triangleSides (C2D.fromVertexes triangleVertexes)
                        
            , test "Line segment length 0 does not return a side"
                     <| compare2dHulls doubleSides (C2D.fromVertexes doubleVertexes)
            ]


compare2dHulls : C2D.Hull -> C2D.Hull -> Assertion
compare2dHulls expected actual =
  assertEqual (C2D.toPrintable expected) (C2D.toPrintable actual)


floorAt : Vec2 -> C2D.Side
floorAt p =
  { keyPoint = p, normal = Vec2.vec2 0 1 }


ceilingAt : Vec2 -> C2D.Side
ceilingAt p =
  { keyPoint = p, normal = Vec2.vec2 0 -1 }


slopeAt : Vec2 -> C2D.Side
slopeAt p =
  { keyPoint = p, normal = Vec2.vec2 -1 1 }


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
  suite "Empty surface"
          [ test "Point is not inside a hull with no faces"
                   <| assert
                   <| C3D.isOutside [[]] (Vec3.vec3 0 0 0)
                      
          , test "Point is not inside a boundary with no hulls"
                   <| assert
                   <| C3D.isOutside [] (Vec3.vec3 0 0 0)
          ]


oneFace : Test
oneFace =
  suite "One-faced hull"
          [ test "Point in -Z is inside out-facing surface on Z=0"
                   <| assert
                   <| C3D.isInside [[ face (0,0,0) (0,0,1) ]] (Vec3.vec3 0 0 -5)

          , test "Point in +Z is inside in-facing surface on Z=0"
                   <| assert
                   <| C3D.isInside [[ face (0,0,0) (0,0,-1) ]] (Vec3.vec3 0 0 5)
          , test "Point in -Y is inside up-facing surface on Y=0"
                   <| assert
                   <| C3D.isInside [[ face (0,0,0) (0,1,0) ]] (Vec3.vec3 0 -5 5)
          , test "Point at X = 5 is outside left-facing surface on X = 10"
                   <| assert
                   <| C3D.isOutside [[ face (10,0,0) (-1,0,0) ]] (Vec3.vec3 5 0 0)
          , test "On the face counts as being inside"
                   <| assert
                   <| C3D.isInside [[ face (1, 1, 1) (0.6, -0.8, 0) ]] (Vec3.vec3 1.003 1.004 1)
          ]


twoFacesOneHull : Test
twoFacesOneHull =
  let
    hull =
      [ [ face (0, 0, 0) (0, 1, 0) 
        , face (0, 0, 0) (1, 0, 0)
        ]
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


twoFacesTwoHulls : Test
twoFacesTwoHulls =
  let
    hull =
      [ [ face (0, 0, 0) (0, 1, 0) ]
      , [ face (0, 0, 0) (1, 0, 0) ]
      ]
  in
    suite "Two one-faced hulls"
            [ test "point is inside if inside both faces"
                     <| assert
                     <| C3D.isInside hull (Vec3.vec3 -10 -10 -10)
            , test "point is outside if outside both faces"
                     <| assert
                     <| C3D.isOutside hull (Vec3.vec3 10 10 -10)
            , test "point is inside if inside only one face"
                     <| assert
                     <| C3D.isInside hull (Vec3.vec3 10 -10 -10)
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
