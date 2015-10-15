module Main where

import Color exposing (Color)
import Graphics.Element as Element
import Graphics.Collage as Collage
import Math.Vector2 as Vec2 exposing (Vec2)
import Mouse
import Text
import String


main : Signal Element.Element
main = 
  Signal.foldp update init input |> Signal.map view


view : Model -> Element.Element
view model = 
  Element.flow Element.down
           [ picture model
           , text model
           ]


update : Update -> Model -> Model
update up model =
  case up of
    CursorAt pos ->
      { model | position <- pos
              , hit <- detectCollision theBoundary pos
      }


input : Signal Update
input =
  let
    toModelSpace (mouseX, mouseY) =
      Vec2.vec2 (toFloat mouseX) (toFloat (negate mouseY))
        |> Vec2.add (Vec2.vec2 -scale scale)
        |> CursorAt
  in
    Signal.map toModelSpace Mouse.position


init : Model
init =
  { position = Vec2.vec2 0 0
  , hit = False
  }


picture : Model -> Element.Element
picture model =
  Collage.collage (2*scale) (2*scale)
           [ edge Color.lightGrey (Vec2.vec2 1 0)
           , edge Color.lightGrey (Vec2.vec2 0 1)
           , drawBounds theBoundary
           ]

    
drawBounds : Boundary -> Collage.Form
drawBounds boundary  =
  let
    drawSide (pointColor, edgeColor) { keyPoint, normal } =
      Collage.group
               [ Collage.circle 3 |> Collage.filled pointColor
               , ray normal
               , edge edgeColor normal
               ]
        |> Collage.move (Vec2.toTuple keyPoint)

    drawHull colors hull =
      Collage.group (List.map (drawSide colors) hull)
  in
    Collage.group (List.map2 drawHull palette boundary)


ray : Vec2 -> Collage.Form
ray vec =
  line Collage.defaultLine (0, 0) (Vec2.toTuple vec)


edge : Color -> Vec2 -> Collage.Form
edge color normal =
  let
    (nX, nY) =
      Vec2.toTuple (Vec2.scale scale normal)
  in
    line (Collage.dashed color) (-nY, nX) (nY, -nX)
         
       
line : Collage.LineStyle -> (Float, Float) -> (Float, Float) -> Collage.Form
line style a b =
  Collage.segment a b |> Collage.traced style
    
         
text : Model -> Element.Element
text model =
  let
    positionString =
      model.position |> Vec2.toTuple |> toString

    hitString =
      if model.hit then "Yes" else "No"

    modelText =
      Text.concat
              [ Text.italic (Text.fromString "Position:  ")
              , Text.fromString positionString
              , Text.italic (Text.fromString "\nHit?  ")
              , Text.fromString hitString
              ]
  in
    modelText |> Element.leftAligned


detectCollision : Boundary -> Vec2 -> Bool
detectCollision boundary testPoint =
  let
    isInside { keyPoint, normal } =
      Vec2.dot normal (Vec2.sub testPoint keyPoint) <= 0
  in
    List.any (List.all isInside) boundary
      
      
theBoundary : Boundary
theBoundary =
  let
    side a b =
      { keyPoint = Vec2.fromTuple a
      , normal = Vec2.fromTuple b
      }
  in
    [ [ side (-60, 20) (10, 0)
      , side (-70, 40) (-3, 10)
      , side (-70, 10) (-10, -10)
      ]
    , [ side (70, -20) (-5, 10)
      , side (70, -80) (5, -10)
      , side (50, -50) (-10, -1)
      , side (90, -50) (10, 1)
      ]
    , [ side (150, 150) (-10, -10)
      ]
    ]


palette : List (Color, Color)
palette =
  [ (Color.red, Color.lightRed)
  , (Color.blue, Color.lightBlue)
  , (Color.yellow, Color.lightYellow)
  , (Color.purple, Color.lightPurple)
  , (Color.orange, Color.lightOrange)
  , (Color.green, Color.lightGreen)
  , (Color.brown, Color.lightBrown)
  ]


scale = 250

  
type Update =
  CursorAt Vec2


type alias Model =
  { position : Vec2
  , hit : Bool
  }

                 
type alias Boundary =
  List Hull

       
type alias Hull =
  List Side

       
type alias Side =
  { keyPoint : Vec2
  , normal : Vec2
  }
