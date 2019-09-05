module Tetromino exposing (Location, Point, Tetromino, i, j, l, main, o, s, t, toForm, toPoint, z)

import Block exposing (Block)
import Collage exposing (..)
import Collage.Render exposing (svg)
import Color exposing (Color)
import Html exposing (Html)
import List


type alias Location =
    ( Int, Int )


type alias Point =
    ( Float, Float )


type alias Tetromino =
    { shape : List Location, block : Block }


toPoint : Location -> Point
toPoint ( m, n ) =
    ( toFloat m * Block.size, toFloat n * Block.size )


toForm : Tetromino -> Collage msg
toForm { shape, block } =
    let
        form =
            Block.toForm block

        translate location =
            shift
                (toPoint location)
                form

        forms =
            List.map translate shape
    in
    group forms


rotateLocation : Point -> Float -> Location -> Location
rotateLocation ( x, y ) angle ( row, col ) =
    let
        rowOrigin =
            toFloat row - x

        colOrigin =
            toFloat col - y

        ( sinValue, cosValue ) =
            ( sin angle, cos angle )

        rowRotated =
            rowOrigin * cosValue - colOrigin * sinValue

        colRotated =
            rowOrigin * sinValue + colOrigin * cosValue
    in
    ( (rowRotated + x) |> round, (colRotated + y) |> round )


calculatePivot : Tetromino -> Point
calculatePivot { shape, block } =
    let
        compareX func ( x, y ) currentValue =
            func currentValue x

        compareY func ( x, y ) currentValue =
            func currentValue y

        -- TODO: the -999 or 999 are a hack. Use a Maybe instead
        maxRowValue =
            List.foldr (compareX max) -999 shape |> toFloat

        minRowValue =
            List.foldr (compareX min) 999 shape |> toFloat

        maxColValue =
            List.foldr (compareY max) -999 shape |> toFloat

        minColValue =
            List.foldr (compareY min) 999 shape |> toFloat

        distance x1 x2 =
            x1 + 1 - x2 |> abs

        maxRowDistance =
            distance maxRowValue minRowValue

        maxColDistance =
            distance maxColValue minColValue
    in
    ( minRowValue + (maxRowDistance / 2), minColValue + (maxColDistance / 2) )


rotate : Tetromino -> Tetromino
rotate tetromino =
    let
        pivot =
            calculatePivot tetromino

        rotateAroundPivot =
            rotateLocation pivot (degrees 90)

        newShape =
            List.map rotateAroundPivot tetromino.shape
    in
    Tetromino newShape tetromino.block


concatTuples : Location -> String -> String
concatTuples ( x, y ) cV =
    cV ++ " (" ++ String.fromInt x ++ ", " ++ String.fromInt y ++ ")"


displayLocations : Tetromino -> String
displayLocations tetromino =
    List.foldr concatTuples "" tetromino.shape


displayPoint : Point -> String
displayPoint ( x, y ) =
    "(" ++ String.fromFloat x ++ ", " ++ String.fromFloat y ++ ")"


main : Html msg
main =
    -- i |> displayLocations |> Html.text
    i |> calculatePivot |> displayPoint |> Html.text



-- constants


i : Tetromino
i =
    { shape = [ ( 0, 1 ), ( 0, 0 ), ( 0, -1 ), ( 0, -2 ) ], block = Block Color.lightBlue }


j : Tetromino
j =
    { shape = [ ( 0, 1 ), ( 0, 0 ), ( 0, -1 ), ( -1, -1 ) ], block = Block Color.darkBlue }


l : Tetromino
l =
    { shape = [ ( 0, 1 ), ( 0, 0 ), ( 0, -1 ), ( 1, -1 ) ], block = Block Color.orange }


o : Tetromino
o =
    { shape = [ ( 0, 1 ), ( 0, 0 ), ( 1, 0 ), ( 1, 1 ) ], block = Block Color.yellow }


s : Tetromino
s =
    { shape = [ ( 0, 0 ), ( 1, 0 ), ( 1, 1 ), ( 2, 1 ) ], block = Block Color.lightGreen }


z : Tetromino
z =
    { shape = [ ( 0, 1 ), ( 1, 1 ), ( 1, 0 ), ( 2, 0 ) ], block = Block Color.red }


t : Tetromino
t =
    { shape = [ ( 0, 0 ), ( 1, 0 ), ( 2, 0 ), ( 1, 1 ) ], block = Block Color.purple }
