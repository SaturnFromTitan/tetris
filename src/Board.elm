module Board exposing (Board, addBlock, addTetromino, background, backgroundColor, checkRow, clearLines, clearRow, cols, isInBounds, isIntersecting, isValid, main, new, rows, testBoard, testTetromino, toForm)

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
        offsetX =
            (toFloat (cols - 1) / 2) * Block.size

        offsetY =
            (toFloat (rows - 1) / 2)
                * Block.size

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



-- collision


isInBounds : Tetromino -> Bool
isInBounds { shape } =
    let
        -- it's intended that there's no validation at the top
        -- it's needed for starting the block and not critical as the user can't move up
        checkLocation ( x, y ) =
            x >= 0 && y >= 0 && x < cols
    in
    List.all checkLocation shape


isIntersecting : Tetromino -> Board -> Bool
isIntersecting { shape } board =
    let
        checkLocation location =
            Dict.member location board
    in
    List.any checkLocation shape


isValid : Tetromino -> Board -> Bool
isValid tetromino board =
    isInBounds tetromino && not (isIntersecting tetromino board)



-- clearing rows
-- TODO: Refactor row/cols inverse to standard notation
--   This is due to x, y coordinate system vs row/col view


checkRow : Int -> Board -> Bool
checkRow row board =
    let
        blocksOnThisRow =
            Dict.filter (\( _, r ) _ -> r == row) board
    in
    Dict.size blocksOnThisRow == cols


clearRow : Int -> Board -> Board
clearRow row board =
    let
        shift ( c, r ) block newBoard =
            if r < row then
                Dict.insert ( c, r ) block newBoard

            else if r > row then
                Dict.insert ( c, r - 1 ) block newBoard

            else
                newBoard
    in
    Dict.foldr shift Dict.empty board


clearLines : Board -> Board
clearLines =
    let
        clearLines_ row board =
            if row >= rows then
                board

            else if checkRow row board then
                clearLines_ row (clearRow row board)

            else
                clearLines_ (row + 1) board
    in
    clearLines_ 0



-- testin'...


testBoard : Board
testBoard =
    new
        [ ( ( 0, 0 ), Block Color.yellow )
        , ( ( 1, 0 ), Block Color.yellow )
        , ( ( 2, 0 ), Block Color.yellow )
        , ( ( 3, 0 ), Block Color.yellow )
        , ( ( 4, 0 ), Block Color.yellow )
        , ( ( 5, 0 ), Block Color.yellow )
        , ( ( 6, 0 ), Block Color.yellow )
        , ( ( 7, 0 ), Block Color.yellow )
        , ( ( 8, 0 ), Block Color.yellow )
        , ( ( 9, 0 ), Block Color.yellow )
        , ( ( 0, 1 ), Block Color.blue )
        , ( ( 1, 1 ), Block Color.blue )
        , ( ( 2, 1 ), Block Color.blue )
        , ( ( 3, 1 ), Block Color.blue )
        , ( ( 4, 1 ), Block Color.blue )
        , ( ( 5, 1 ), Block Color.blue )
        , ( ( 6, 1 ), Block Color.blue )
        , ( ( 7, 1 ), Block Color.blue )
        , ( ( 8, 1 ), Block Color.blue )
        , ( ( 9, 1 ), Block Color.blue )
        ]


testTetromino =
    l |> Tetromino.shift ( 0, 1 )


main : Html msg
main =
    testBoard |> clearLines |> toForm |> svg
