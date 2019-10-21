module Block exposing (Block, size, toForm)

import Collage exposing (Collage, filled, group, outlined, solid, square, thin, uniform)
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
