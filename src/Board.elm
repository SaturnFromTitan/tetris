module Board exposing (Board, addBlock, addTetromino, background, backgroundColor, cols, main, new, rows, testBoard, testTetromino, toForm)

import Block exposing (..)
import Collage exposing (..)
import Collage.Render exposing (svg)
import Color exposing (Color)
import Dict exposing (Dict)
import Html exposing (Html)
import Tetromino exposing (..)


type alias Board =
    Dict Location Block


new : List ( Location, Block ) -> Board
new =
    Dict.fromList


cols : Int
cols =
    10


rows : Int
rows =
    20


backgroundColor : Color
backgroundColor =
    Color.black


background : Collage msg
background =
    let
        shape =
            rectangle (toFloat cols * Block.size) (toFloat rows * Block.size)

        border =
            outlined (solid thick (uniform backgroundColor)) shape

        filledShape =
            shape |> filled (uniform backgroundColor)
    in
    group [ filledShape, border ]


addBlock : Location -> Block -> Collage msg -> Collage msg
addBlock ( m, n ) block form =
    let
        {-
           offsetX =
               (toFloat (cols - 1) / 2) * Block.size

           offsetY =
               (toFloat (rows - 1) / 2) * Block.size
        -}
        offsetX =
            0.5 * Block.size

        offsetY =
            0.5 * Block.size

        x =
            toFloat m * Block.size

        y =
            toFloat n * Block.size

        shiftedBlockForm =
            Block.toForm block
                |> Collage.shift ( x - offsetX, y - offsetY )
    in
    group [ shiftedBlockForm, form ]


toForm : Board -> Collage msg
toForm board =
    Dict.foldl addBlock background board


addTetromino : Tetromino -> Board -> Board
addTetromino { shape, block } board =
    let
        toTuple a b =
            ( a, b )

        asBoard =
            List.map2 toTuple shape (List.repeat (List.length shape) block)
                |> new
    in
    Dict.union asBoard board



-- testin'...


testBoard : Board
testBoard =
    new []


testTetromino =
    l |> Tetromino.shift ( 0, 1 )


main : Html msg
main =
    testBoard |> addTetromino testTetromino |> toForm |> svg
