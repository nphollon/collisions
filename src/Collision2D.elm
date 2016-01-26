module Collision2D (isOutside, isInside, fromVectors, Hull, Vec2, vec2) where

{-| Collision detection in two dimensions

# Collision Detection
@docs isInside, isOutside

# Building a Hull
@docs Vector, Hull, vec2, fromVectors

-}

import Vec2


{-| -}
type alias Vec2 =
    Vec2.Vec2


vec2 : Float -> Float -> Vec2
vec2 =
    Vec2.vec2


{-| Given the vertexes of a polygon, compute a hull. Vertexes must be ordered
counter-clockwise around the center of the shape. Only works for convex polygons.

Returns an empty hull if given less than three vertexes.
-}
fromVectors : List Vec2 -> Hull
fromVectors vertexes =
    let
        segments =
            (List.drop 1 vertexes)
                ++ (List.take 1 vertexes)
                |> List.map2 (,) vertexes
    in
        if List.length vertexes >= 3 then
            fromSegments segments
        else
            Bounded []


fromSegments : List ( Vec2, Vec2 ) -> Hull
fromSegments segments =
    let
        toNormal a b =
            let
                ( x, y ) = Vec2.toTuple (Vec2.direction a b)
            in
                Vec2.fromTuple ( -y, x )

        toSide ( a, b ) =
            { keyPoint = a
            , normal = toNormal a b
            }

        isDefined vec =
            not (isNaN (Vec2.getX vec) || isNaN (Vec2.getY vec))
    in
        List.map toSide segments
            |> List.filter (.normal >> isDefined)
            |> Bounded


{-| Returns `True` if the given position is on or in the given hull.
Defaults to `False` if the hull has no sides.

    import Math.Vector2 exposing (vec2)

    hull =
      fromVertexes
        [ vec2 0 1
        , vec2 3 4
        , vec2 1 0
        ]

    isInside hull (vec2 1 1) == True

    isInside hull (vec2 -1 2) == False

    isInside hull (vec2 0 0) == True

-}
isInside : Vec2 -> Hull -> Bool
isInside point (Bounded sides) =
    let
        isBehind side =
            Vec2.dot side.normal (Vec2.sub point side.keyPoint) < 1.0e-6
    in
        not (List.isEmpty sides) && List.all isBehind sides


{-| Returns `True` if the given position is outside the given hull.
The logical inverse of `isInside`.
-}
isOutside : Vec2 -> Hull -> Bool
isOutside point boundary =
    not (isInside point boundary)


{-| A collection of sides that together represent a hull. This library
interprets the sides as forming the smallest possible convex polygon.
-}
type Hull
    = Bounded (List Side)



{- A side is a straight-line boundary with an inside and an outside. The key
point is a location on the boundary. The
[normal](https://en.wikipedia.org/wiki/Normal_%28geometry%29) is a unit vector
perpendicular to the boundary.

Any point on the side can be a key point, so sides with different key points can
be equivalent sometimes.

Since the sides have no endpoints, infinite-area hulls are possible (e.g. if
there are less than three sides).
-}


type alias Side =
    { keyPoint : Vec2
    , normal : Vec2
    }
