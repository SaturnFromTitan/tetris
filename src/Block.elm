module Block exposing (Block, borderColor, main, size, toForm)

import Collage exposing (..)
import Collage.Render exposing (svg)
import Color exposing (Color)
import Html exposing (Html)


type alias Block =
    { color : Color }


size : Float
size =
    25


borderColor : Color
borderColor =
    Color.black


toForm : Block -> Collage msg
toForm block =
    let
        shape =
            square size

        border =
            outlined (solid thick (uniform borderColor)) shape

        filledShape =
            shape |> filled (uniform block.color)
    in
    group [ filledShape, border ]


main : Html msg
main =
    toForm (Block Color.blue) |> svg
