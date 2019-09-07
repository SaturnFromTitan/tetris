module Block exposing (Block, main, size, toForm)

import Collage exposing (..)
import Collage.Render exposing (svg)
import Color exposing (Color)
import Html exposing (Html)


type alias Block =
    { color : Color }


size : Float
size =
    25


toForm : Block -> Collage msg
toForm block =
    let
        shape =
            square size

        border =
            outlined (solid thin (uniform Color.white)) shape

        filledShape =
            shape |> filled (uniform block.color)
    in
    group [ border, filledShape ]


main : Html msg
main =
    toForm (Block Color.blue) |> svg
