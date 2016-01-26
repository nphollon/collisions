module Vec2 (Vec2, vec2, toTuple, fromTuple, fromRecord, dot, normalize, add, sub, getX, getY, scale, length, lengthSquared, direction) where


type Vec2
    = Vec2 Float Float


vec2 : Float -> Float -> Vec2
vec2 x y =
    Vec2 x y


fromTuple : ( Float, Float ) -> Vec2
fromTuple ( x, y ) =
    vec2 x y


toTuple : Vec2 -> ( Float, Float )
toTuple (Vec2 x y) =
    ( x, y )


fromRecord : { x : Float, y : Float } -> Vec2
fromRecord { x, y } =
    vec2 x y


dot : Vec2 -> Vec2 -> Float
dot (Vec2 u1 u2) (Vec2 v1 v2) =
    u1 * v1 + u2 * v2


normalize : Vec2 -> Vec2
normalize v =
    scale (1 / length v) v


add : Vec2 -> Vec2 -> Vec2
add (Vec2 u1 u2) (Vec2 v1 v2) =
    vec2 (u1 + v1) (u2 + v2)


sub : Vec2 -> Vec2 -> Vec2
sub (Vec2 u1 u2) (Vec2 v1 v2) =
    vec2 (u1 - v1) (u2 - v2)


getX : Vec2 -> Float
getX (Vec2 x _) =
    x


getY : Vec2 -> Float
getY (Vec2 _ y) =
    y


scale : Float -> Vec2 -> Vec2
scale a (Vec2 x y) =
    vec2 (a * x) (a * y)


length : Vec2 -> Float
length =
    lengthSquared >> sqrt


lengthSquared : Vec2 -> Float
lengthSquared v =
    v `dot` v


direction : Vec2 -> Vec2 -> Vec2
direction a b =
    normalize (a `sub` b)
