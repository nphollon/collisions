module Main where

import ElmTest.Test exposing (Test, test, suite)
import ElmTest.Assertion exposing (assert)
import ElmTest.Runner.Element as GraphicsRunner
import ElmTest.Runner.Console as ConsoleRunner
import IO.Runner

import Math.Vector2 as Vec2 exposing (Vec2)

import Collision2D as C2D



allTests : Test
allTests =
  suite "2D collisions"
          [ noSides
          , oneSide
          , twoSidesOneHull
          , twoSidesTwoHulls
          , badArguments
          ]


noSides : Test
noSides =
  suite "Empty boundary"
          [ test "Point is not inside a boundary with no sides"
                   <| assert
                   <| C2D.isOutside [[]] (Vec2.vec2 0 0)                  
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


badArguments : Test
badArguments =
  let
    nullNormal =
      [[ { keyPoint = Vec2.vec2 0 0, normal = Vec2.vec2 0 0 } ]]
  in
    suite "Malformed arguments"
            [ test "Point always outside a side with null normal"
                     <| assert
                     <| C2D.isOutside nullNormal (Vec2.vec2 0 0)
            ]
    
            
floorAt : Vec2 -> C2D.Side
floorAt p =
  { keyPoint = p, normal = Vec2.vec2 0 1 }


ceilingAt : Vec2 -> C2D.Side
ceilingAt p =
  { keyPoint = p, normal = Vec2.vec2 0 -1 }


slopeAt : Vec2 -> C2D.Side
slopeAt p =
  { keyPoint = p, normal = Vec2.vec2 -1 1 }

        
port requests : Signal IO.Runner.Request
port requests =
  IO.Runner.run responses (ConsoleRunner.runDisplay allTests)
    

port responses : Signal IO.Runner.Response
