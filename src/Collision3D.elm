module Collision3D (fromTriangles,
                    Hull, Face) where

import Math.Vector3 as Vec3 exposing (Vec3)


fromTriangles : List (Vec3, Vec3, Vec3) -> Hull
fromTriangles =
  List.map (\(a, b, c) ->
              { normal =
                  Vec3.cross (Vec3.sub a b) (Vec3.sub b c)
                    |> Vec3.normalize
              , keyPoint = a
              }
           )


toPrintable : Hull -> List { keyPoint : Triple Float, normal : Triple Float }
toPrintable =
  List.map (\face ->
              { keyPoint = Vec3.toTuple face.keyPoint
              , normal = Vec3.toTuple face.normal
              }
           )


type alias Hull =
  List Face

       
type alias Face =
  { keyPoint : Vec3, normal : Vec3 }

  
type alias Triple a =
  (a, a, a)
