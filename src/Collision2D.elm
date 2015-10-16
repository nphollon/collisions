module Collision2D (isOutside, isInside, Boundary, Side) where

import Math.Vector2 as Vec2 exposing (Vec2)


isOutside : Boundary -> Vec2 -> Bool
isOutside boundary point = not (isInside boundary point)

                           
isInside : Boundary -> Vec2 -> Bool
isInside boundary point =
  let
    isBehind side =
      Vec2.dot side.normal (Vec2.sub point side.keyPoint) < 0 
  in
    boundary
      |> List.filter (not << List.isEmpty)
      |> List.any (List.all isBehind)


type alias Boundary =
  List ( List Side )

type alias Side =
  { keyPoint : Vec2, normal : Vec2 }
