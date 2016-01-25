module Main (..) where

import Color exposing (Color)
import Graphics.Element as Element
import Graphics.Collage as Collage
import Mouse
import Text
import String
import Collision2D exposing (Vector)


main : Signal Element.Element
main =
    Signal.foldp update init input |> Signal.map view



{- DATA -}


type alias Model =
    { position : Vector
    , hit : Bool
    }


init : Model
init =
    { position = vec2 0 0
    , hit = False
    }


theShapes : List (List ( Float, Float ))
theShapes =
    [ [ ( -90, 30 ), ( -70, -50 ), ( -10, 50 ) ]
    , [ ( 50, 25 ), ( 50, -85 ), ( 90, -75 ), ( 90, 15 ) ]
    , [ ( 90, 15 ), ( 90, -75 ), ( 110, -95 ), ( 120, 0 ) ]
    ]


scale =
    150



{- UPDATE LOGIC -}


type Update
    = CursorAt Vector


input : Signal Update
input =
    let
        toModelSpace ( mouseX, mouseY ) =
            vec2 (toFloat mouseX) (toFloat (negate mouseY))
                |> vec2Add (vec2 -scale scale)
                |> CursorAt
    in
        Signal.map toModelSpace Mouse.position


update : Update -> Model -> Model
update up model =
    case up of
        CursorAt pos ->
            { model
                | position = pos
                , hit = List.any (Collision2D.isInside pos) theBoundary
            }


theBoundary : List Collision2D.Hull
theBoundary =
    List.map (List.map vec2FromTuple >> Collision2D.fromVectors) theShapes


vec2FromTuple : ( Float, Float ) -> Vector
vec2FromTuple ( x, y ) =
    { x = x, y = y }


vec2ToTuple : Vector -> ( Float, Float )
vec2ToTuple { x, y } =
    ( x, y )


vec2 : Float -> Float -> Vector
vec2 x y =
    { x = x, y = y }


vec2Add : Vector -> Vector -> Vector
vec2Add a b =
    { x = a.x + b.x
    , y = a.y + b.y
    }


vec2Scale : Float -> Vector -> Vector
vec2Scale a v =
    { x = v.x * a
    , y = v.y * a
    }



{- VIEW LOGIC -}


view : Model -> Element.Element
view model =
    Element.flow
        Element.down
        [ picture model
        , text model
        ]


picture : Model -> Element.Element
picture model =
    Collage.collage
        (2 * scale)
        (2 * scale)
        [ edge Color.lightGrey (vec2 1 0)
        , edge Color.lightGrey (vec2 0 1)
        , drawShapes theShapes
        ]


drawShapes : List (List ( Float, Float )) -> Collage.Form
drawShapes shapes =
    let
        drawShape ( _, fillColor ) vecs =
            vecs
                |> Collage.polygon
                |> Collage.filled fillColor
    in
        Collage.group (List.map2 drawShape palette shapes)


ray : Vector -> Collage.Form
ray vec =
    line Collage.defaultLine ( 0, 0 ) (vec2ToTuple vec)


edge : Color -> Vector -> Collage.Form
edge color normal =
    let
        ( nX, nY ) =
            vec2ToTuple (vec2Scale scale normal)
    in
        line (Collage.dashed color) ( -nY, nX ) ( nY, -nX )


line : Collage.LineStyle -> ( Float, Float ) -> ( Float, Float ) -> Collage.Form
line style a b =
    Collage.segment a b |> Collage.traced style


text : Model -> Element.Element
text model =
    let
        positionString =
            model.position |> vec2ToTuple |> toString

        hitString =
            if model.hit then
                "Yes"
            else
                "No"

        modelText =
            Text.concat
                [ Text.italic (Text.fromString "Position:  ")
                , Text.fromString positionString
                , Text.italic (Text.fromString "\nHit?  ")
                , Text.fromString hitString
                ]
    in
        modelText |> Element.leftAligned


palette : List ( Color, Color )
palette =
    [ ( Color.red, Color.lightRed )
    , ( Color.blue, Color.lightBlue )
    , ( Color.yellow, Color.lightYellow )
    , ( Color.purple, Color.lightPurple )
    , ( Color.orange, Color.lightOrange )
    , ( Color.green, Color.lightGreen )
    , ( Color.brown, Color.lightBrown )
    ]
