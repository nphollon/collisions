module Collision2D (isOutside, isInside, fromSegments, fromVertexes, toPrintable,
                    Hull, Side) where

{-|
# Types
@docs Hull, Side, Vec2

# Collision Detection
@docs isInside, isOutside

# Hull Construction
@docs fromSegments, fromVertexes

# Helpers
@docs toPrintable
-}


import Math.Vector2 as Vec2


{-| A side is a straight-line boundary with an inside and an outside. The key point is a location on the boundary. The [normal](https://en.wikipedia.org/wiki/Normal_%28geometry%29)
is a unit vector perpendicular to the boundary.

Any point on the side can be a key point, so sides with different key points can be equivalent sometimes.

    -- These sides are equivalent because their key points are on the same line
    { keyPoint = Vec2.vec2 1 0, normal = Vec2.vec2 0 1 } ==
      { keyPoint = Vec2.vec2 -1 0, normal = Vec2.vec2 0 1 }

    -- These sides are parallel because their normals are equal but their key points are on different lines
    { keyPoint = Vec2.vec2 2 1, normal = Vec2.vec2 0.6 0.8 } ==
      { keyPoint = Vec2.vec2 -3 -3, normal = Vec2.vec2 0.6 0.8 }

-}

type alias Side =
  { keyPoint : Vec2
  , normal : Vec2 }


{-| A collection of sides that together represent a shape. This library interprets the sides as forming the smallest possible convex polygon.

Since the sides have no endpoints, infinite-area hulls are possible (e.g. if there are less than three sides).

-}
type alias Hull =
  List Side


{-| Alias for Vec2 in [johnpmayer/elm-linear-algebra](http://package.elm-lang.org/packages/johnpmayer/elm-linear-algebra/2.0.2)
-}
type alias Vec2 = Vec2.Vec2

                
{-| Returns `True` if the given position is on or in the given hull.
Defaults to `False` if the hull has no sides.
    
    hull =
      [ { keyPoint = Vec2.vec2 0 1, normal = Vec2.vec2 -1 0 }
      , { keyPoint = Vec2.vec2 3 4, normal = Vec2.vec2 0.8 0.6 }
      , { keyPoint = Vec2.vec2 1 0, normal = Vec2.vec2 0 -1 }
      ]

    isInside hull (Vec2.vec2 1 1) == True

    isInside hull (Vec2.vec2 -1 2) == False

    isInside hull (Vec2.vec2 0 0) == True

-}
isInside : Hull -> Vec2 -> Bool
isInside boundary point =
  let
    isBehind side =
      Vec2.dot side.normal (Vec2.sub point side.keyPoint) < 1e-6
  in
    not (List.isEmpty boundary) && List.all isBehind boundary


{-| Returns `True` if the given position is outside the given hull.
The logical inverse of `isInside`.
-}
isOutside : Hull -> Vec2 -> Bool
isOutside boundary point = not (isInside boundary point)


{-| Given the vertexes of a polygon, compute a hull. Vertexes must be ordered
counter-clockwise around the center of the shape. Only works for convex polygons.

Returns an empty hull if given less than three vertexes.
-}
fromVertexes : List Vec2 -> Hull
fromVertexes vertexes =
  let
    segments =
      (List.drop 1 vertexes) ++ (List.take 1 vertexes)
        |> List.map2 (,) vertexes
  in
    if | List.length vertexes >= 3 -> fromSegments segments
       | otherwise -> []


{-| Given a list of line segments, compute a hull. For a line segment (a,b),
the normal will point counter-clockwise.

    fromSegments [(Vec2.vec2 1 1, Vec2.vec2 2 2)] ==
      { keyPoint = Vec2.vec2 1 1, normal = Vec2.vec2 -0.7071 -0.7071 }
-}
fromSegments : List (Vec2, Vec2) -> Hull
fromSegments segments =
  let
    toNormal a b =
      let (x, y) = Vec2.toTuple (Vec2.direction a b)
      in Vec2.fromTuple (y, -x)

    toSide (a, b) =
      { keyPoint = a
      , normal = toNormal a b
      }

    isDefined vec =
      not (isNaN (Vec2.getX vec) || isNaN (Vec2.getY vec))
  in
    List.map toSide segments
      |> List.filter (.normal >> isDefined)


{-| Translates a hull to a form compatible with `toString`. Useful for debugging.
-}
toPrintable : Hull -> List { keyPoint : (Float, Float), normal : (Float, Float) }
toPrintable =
  List.map (\face ->
              { keyPoint = Vec2.toTuple face.keyPoint
              , normal = Vec2.toTuple face.normal
              }
           )

