module Upcoming exposing (toForm)

import Block exposing (Block)
import Collage exposing (Collage, LineCap(..), filled, group, outlined, shift, solid, square, uniform)
import Color exposing (Color)
import Tetromino exposing (Point, Tetromino, toForm)


centerShift : Point -> Point
centerShift ( a, b ) =
    ( -a * Block.size, -b * Block.size )


toForm : Int -> Tetromino -> Collage msg
toForm width next =
    let
        boxShapeWidth =
            toFloat width - 10

        boxShape =
            square boxShapeWidth

        lineStyle =
            solid 3.0 (uniform Color.black)

        lineStyle_ =
            { lineStyle | cap = Round }

        border =
            outlined lineStyle_ boxShape

        filledBoxShape =
            boxShape |> filled (uniform Color.white)

        tetrominoForm =
            Tetromino.toForm next
                |> shift (centerShift next.pivot)
    in
    group
        [ border
        , tetrominoForm
        , filledBoxShape
        ]
