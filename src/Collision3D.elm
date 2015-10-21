module Collision3D (isOutside, isInside, fromTriangles, toPrintable,
                    Hull, Face) where

import Math.Vector3 as Vec3 exposing (Vec3)


type alias Hull =
  List Face

       
type alias Face =
  { keyPoint : Vec3
  , normal : Vec3
  }


isOutside : Hull -> Vec3 -> Bool
isOutside boundary point = not (isInside boundary point)


isInside : Hull -> Vec3 -> Bool
isInside boundary point =
  let
    isBehind face =
      Vec3.dot face.normal (Vec3.sub point face.keyPoint) < 1e-6
  in
    not (List.isEmpty boundary) && List.all isBehind boundary


fromTriangles : List (Vec3, Vec3, Vec3) -> Hull
fromTriangles triangles =
  let
    toFace (a, b, c) =
      { normal =
          Vec3.normalize (Vec3.cross (Vec3.sub a b) (Vec3.sub b c))
      , keyPoint = a
      }

    isDefined vec =
      [ Vec3.getX, Vec3.getY, Vec3.getZ ]
        |> List.map (\f -> f vec)
        |> List.all (not << isNaN)
  in
    List.map toFace triangles
      |> List.filter (.normal >> isDefined)


toPrintable : Hull -> List { keyPoint : (Float, Float, Float), normal : (Float, Float, Float) }
toPrintable =
  List.map (\face ->
              { keyPoint = Vec3.toTuple face.keyPoint
              , normal = Vec3.toTuple face.normal
              }
           )
