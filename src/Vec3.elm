module Vec3 (Vec3, vec3, fromTuple, fromRecord, dot, normalize, cross, sub, getX, getY, getZ, scale, length, lengthSquared) where


type Vec3
    = Vec3 Float Float Float


vec3 : Float -> Float -> Float -> Vec3
vec3 x y z =
    Vec3 x y z


fromTuple : ( Float, Float, Float ) -> Vec3
fromTuple ( x, y, z ) =
    vec3 x y z


fromRecord : { x : Float, y : Float, z : Float } -> Vec3
fromRecord { x, y, z } =
    vec3 x y z


normalize : Vec3 -> Vec3
normalize v =
    scale (1 / length v) v


scale : Float -> Vec3 -> Vec3
scale a (Vec3 x y z) =
    vec3 (a * x) (a * y) (a * z)


length : Vec3 -> Float
length =
    lengthSquared >> sqrt


lengthSquared : Vec3 -> Float
lengthSquared v =
    v `dot` v


dot : Vec3 -> Vec3 -> Float
dot (Vec3 u1 u2 u3) (Vec3 v1 v2 v3) =
    u1 * v1 + u2 * v2 + u3 * v3


cross : Vec3 -> Vec3 -> Vec3
cross (Vec3 u1 u2 u3) (Vec3 v1 v2 v3) =
    vec3
        (u2 * v3 - u3 * v2)
        (u3 * v1 - u1 * v3)
        (u1 * v2 - u2 * v1)


sub : Vec3 -> Vec3 -> Vec3
sub (Vec3 u1 u2 u3) (Vec3 v1 v2 v3) =
    vec3 (u1 - v1) (u2 - v2) (u3 - v3)


getX : Vec3 -> Float
getX (Vec3 x _ _) =
    x


getY : Vec3 -> Float
getY (Vec3 _ y _) =
    y


getZ : Vec3 -> Float
getZ (Vec3 _ _ z) =
    z
