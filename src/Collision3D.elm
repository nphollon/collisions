module Collision3D (isOutside, isInside, fromTriangles, toPrintable,
                    Hull, Face, Vec3) where

{-| Collision detection in three dimensions

# Types
@docs Hull, Side, Vec3

# Collision Detection
@docs isInside, isOutside

# Hull Construction
@docs fromTriangles

# Helpers
@docs toPrintable
-}


import Math.Vector3 as Vec3


{-| A face is a plane boundary with an inside and an outside. The key point is a
location on the boundary. The [normal](https://en.wikipedia.org/wiki/Normal_%28geometry%29)
is a unit vector perpendicular to the boundary.

Any point on the face can be a key point, so faces with different key points can 
be equivalent sometimes.

    -- These faces are equivalent because their key points are on the same plane
    { keyPoint = Vec3.vec3 1 0 -1, normal = Vec3.vec3 0 1 0 } ==
      { keyPoint = Vec3.vec2 -1 0 3, normal = Vec3.vec3 0 1 0 }

    -- These faces are parallel because their normals are equal
    -- and their key points are on different lines
    { keyPoint = Vec3.vec3 2 1 0, normal = Vec3.vec3 0 0.6 0.8 } ==
      { keyPoint = Vec2.vec2 -3 -3 -3, normal = Vec3.vec3 0 0.6 0.8 }
-}
type alias Face =
  { keyPoint : Vec3
  , normal : Vec3
  }


{-| A collection of faces that together represent a shape. This library interprets
the sides as forming the smallest possible convex polyhedron.

Since the faces have no edges, infinite-volume hulls are possible (e.g. if there are
less than four faces).
-}
type alias Hull =
  List Face


{-| Alias for Vec3 in [johnpmayer/elm-linear-algebra](http://package.elm-lang.org/packages/johnpmayer/elm-linear-algebra/2.0.2)
-}
type alias Vec3 = Vec3.Vec3


{-| Returns `True` if the given position is on or in the given hull.
Defaults to `False` if the hull has no sides.

    hull =
      [ { keyPoint = Vec3.vec3 2 0 5, normal = Vec3.vec3 0 -1 0 }
      , { keyPoint = Vec3.vec3 1 1 0, normal = Vec3.vec3 0 0 -1 }
      , { keyPoint = Vec3.vec3 0 0 0, normal = Vec3.vec3 -1 0 0 }
      , { keyPoint = Vec3.vec3 9 9 9, normal = Vec3.vec3 0.5574 0.5574 0.5574 }
      ]

    isInside hull (Vec3.vec3 5 1 1) == True

    isInside hull (Vec3.vec3 -1 2 -1) == False

    isInside hull (Vec3.vec3 0 0 0) == True
-}
isInside : Hull -> Vec3 -> Bool
isInside boundary point =
  let
    isBehind face =
      Vec3.dot face.normal (Vec3.sub point face.keyPoint) < 1e-6
  in
    not (List.isEmpty boundary) && List.all isBehind boundary


{-| Returns `True` if the given position is outside the given hull.
The logical inverse of `isInside`.
-}
isOutside : Hull -> Vec3 -> Bool
isOutside boundary point = not (isInside boundary point)


{-| Given a list of triangles, compute a hull. For a triangle of points (a,b,c),
the resulting normal will be the normalized cross product `(a,b) x (b,c)`.

    fromTriangles [(Vec3.vec3 0 0 0, Vec3.vec3 4 3 0, Vec3.vec3 0 0 10)] ==
      { keyPoint = Vec3.vec3 0 0 0, normal = Vec3.vec3 0.6 0.8 0 }
-}
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


{-| Translates a hull to a form compatible with `toString`. Useful for debugging.
-}
toPrintable : Hull -> List { keyPoint : (Float, Float, Float), normal : (Float, Float, Float) }
toPrintable =
  List.map (\face ->
              { keyPoint = Vec3.toTuple face.keyPoint
              , normal = Vec3.toTuple face.normal
              }
           )
