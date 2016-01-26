module Demo2D (main) where

import Color exposing (Color)
import Graphics.Element as Element
import Graphics.Collage as Collage
import Mouse
import Text
import String
import Vec2
import Collision2D


main : Signal Element.Element
main =
    Signal.foldp update init input |> Signal.map view



{- DATA -}


type alias Model =
    { position : Vec2.Vec2
    , hit : Bool
    }


init : Model
init =
    { position = Vec2.vec2 0 0
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
    = CursorAt Vec2.Vec2


input : Signal Update
input =
    let
        toModelSpace ( mouseX, mouseY ) =
            Vec2.vec2 (toFloat mouseX) (toFloat (negate mouseY))
                |> Vec2.add (Vec2.vec2 -scale scale)
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
    List.map (List.map Vec2.fromTuple >> Collision2D.fromVectors) theShapes



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
        [ edge Color.lightGrey (Vec2.vec2 1 0)
        , edge Color.lightGrey (Vec2.vec2 0 1)
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


ray : Vec2.Vec2 -> Collage.Form
ray vec =
    line Collage.defaultLine ( 0, 0 ) (Vec2.toTuple vec)


edge : Color -> Vec2.Vec2 -> Collage.Form
edge color normal =
    let
        ( nX, nY ) =
            Vec2.toTuple (Vec2.scale scale normal)
    in
        line (Collage.dashed color) ( -nY, nX ) ( nY, -nX )


line : Collage.LineStyle -> ( Float, Float ) -> ( Float, Float ) -> Collage.Form
line style a b =
    Collage.segment a b |> Collage.traced style


text : Model -> Element.Element
text model =
    let
        positionString =
            model.position |> Vec2.toTuple |> toString

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
