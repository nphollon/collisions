module Collision3D (isOutside, isInside, fromTriangles, Hull, Vec3, vec3) where

{-| Collision detection in three dimensions

# Building a Hull
@docs Vec3, Hull, vec3, fromTriangles

# Collision Detection
@docs isInside, isOutside
-}

import Vec3


{-| A vector with x, y, and z components
-}
type alias Vec3 =
    Vec3.Vec3


{-| Build a Vec3 given x, y, and z

    vec3 1 2 3 -- x = 1, y = 2, z = 3
-}
vec3 : Float -> Float -> Float -> Vec3
vec3 =
    Vec3.vec3


{-| Given a list of triangles, compute a hull. For a triangle of points (a,b,c),
the resulting normal will be the normalized cross product `(a to b) x (b to c)`. In
other words, if the triangle vertexes are going counter-clockwise from your point
of view, the normal will be pointing towards from you.

The triangles passed to this function should form a polyhedron that is
* convex (no dents)
* closed (no holes)
-}
fromTriangles : List ( Vec3, Vec3, Vec3 ) -> Hull
fromTriangles triangles =
    let
        toFace ( a, b, c ) =
            { normal =
                Vec3.normalize (Vec3.cross (Vec3.sub b a) (Vec3.sub c a))
            , keyPoint = a
            }

        isDefined vec =
            [ Vec3.getX, Vec3.getY, Vec3.getZ ]
                |> List.map (\f -> f vec)
                |> List.all (not << isNaN)
    in
        List.map toFace triangles
            |> List.filter (.normal >> isDefined)
            |> Bounded


{-| Returns `True` if the given position is on or in the given hull.
Defaults to `False` if the hull has no sides.

    hull =
      fromTriangles
        [ (vec3 0 0 0, vec3 0 0 5, vec3 10 0 0)
        , (vec3 0 0 0, vec3 0 5 0, vec3 0 0 5)
        , (vec3 0 0 0, vec3 10 0 0, vec3 0 5 0)
        , (vec3 10 0 0, vec3 0 0 5, vec3 0 5 0)
        ]

    isInside hull (vec3 5 1 1) == True

    isInside hull (vec3 -1 2 -1) == False

    isInside hull (vec3 0 0 0) == True
-}
isInside : Vec3 -> Hull -> Bool
isInside point (Bounded faces) =
    let
        isBehind face =
            Vec3.dot face.normal (Vec3.sub point face.keyPoint) < 1.0e-6
    in
        not (List.isEmpty faces) && List.all isBehind faces


{-| Returns `True` if the given position is outside the given hull.
The logical inverse of `isInside`.
-}
isOutside : Vec3 -> Hull -> Bool
isOutside point boundary =
    not (isInside point boundary)


{-| A collection of faces that together represent a shape. This library interprets
the faces as forming the smallest possible convex polyhedron.
-}
type Hull
    = Bounded (List Face)



{- A face is a plane boundary with an inside and an outside. The key point is a
location on the boundary. The [normal](https://en.wikipedia.org/wiki/Normal_%28geometry%29)
is a unit vector perpendicular to the boundary.

Any point on the face can be a key point, so faces with different key points can
be equivalent sometimes.

Since the faces have no edges, infinite-volume hulls are possible (e.g. if there are
less than four faces).
-}


type alias Face =
    { keyPoint : Vec3
    , normal : Vec3
    }
