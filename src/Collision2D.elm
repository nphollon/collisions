module Collision2D (isOutside, isInside, fromSegments, fromVertexes,
                             Hull, Side) where

import Math.Vector2 as Vec2 exposing (Vec2)


isOutside : List Hull -> Vec2 -> Bool
isOutside boundary point = not (isInside boundary point)

                           
isInside : List Hull -> Vec2 -> Bool
isInside boundary point =
  let
    isBehind side =
      Vec2.dot side.normal (Vec2.sub point side.keyPoint) < 0 
  in
    boundary
      |> List.filter (not << List.isEmpty)
      |> List.any (List.all isBehind)


fromSegments : List (Vec2, Vec2) -> Hull
fromSegments =
  let
    toNormal a b =
      let (x, y) = Vec2.toTuple (Vec2.direction a b)
      in Vec2.fromTuple (y, -x)

    toSide (a, b) =
      { keyPoint = a
      , normal = toNormal a b
      }
  in
    List.map toSide


fromVertexes : List Vec2 -> Hull
fromVertexes vertexes =
  let
    segments =
      (List.drop 1 vertexes) ++ (List.take 1 vertexes)
        |> List.map2 (,) vertexes
  in
    if | List.length vertexes >= 3 -> fromSegments segments
       | otherwise -> []

               
type alias Hull =
  List Side

type alias Side =
  { keyPoint : Vec2, normal : Vec2 }
