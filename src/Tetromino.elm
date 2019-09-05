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


toPoint : Location -> Point
toPoint ( m, n ) =
    ( toFloat m * Block.size, toFloat n * Block.size )


type alias Tetromino =
    { shape : List Location, block : Block, pivot : Point }


toForm : Tetromino -> Collage msg
toForm { shape, block } =
    let
        form =
            Block.toForm block

        translate location =
            shift
                (toPoint location)
                form

        mappedForms =
            List.map translate shape
    in
    group mappedForms


rotateAroundPoint : Point -> Float -> Location -> Location
rotateAroundPoint ( x, y ) angle ( row, col ) =
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


rotate : Tetromino -> Tetromino
rotate { shape, block, pivot } =
    let
        rotateAroundPivot =
            rotateAroundPoint pivot (degrees 90)

        newShape =
            List.map rotateAroundPivot shape
    in
    Tetromino newShape block pivot


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
    i |> toForm |> svg



-- constants


i : Tetromino
i =
    { shape = [ ( 0, 1 ), ( 0, 0 ), ( 0, -1 ), ( 0, -2 ) ]
    , block = Block Color.lightBlue
    , pivot = ( 0.5, -0.5 )
    }


j : Tetromino
j =
    { shape = [ ( 0, 1 ), ( 0, 0 ), ( 0, -1 ), ( -1, -1 ) ]
    , block = Block Color.darkBlue
    , pivot = ( 0, 0 )
    }


l : Tetromino
l =
    { shape = [ ( 0, 1 ), ( 0, 0 ), ( 0, -1 ), ( 1, -1 ) ]
    , block = Block Color.orange
    , pivot = ( 0, 0 )
    }


o : Tetromino
o =
    { shape = [ ( 0, 1 ), ( 0, 0 ), ( 1, 0 ), ( 1, 1 ) ]
    , block = Block Color.yellow
    , pivot = ( 0, 0 )
    }


s : Tetromino
s =
    { shape = [ ( 0, 0 ), ( 1, 0 ), ( 1, 1 ), ( 2, 1 ) ]
    , block = Block Color.lightGreen
    , pivot = ( 0, 0 )
    }


z : Tetromino
z =
    { shape = [ ( 0, 1 ), ( 1, 1 ), ( 1, 0 ), ( 2, 0 ) ]
    , block = Block Color.red
    , pivot = ( 0, 0 )
    }


t : Tetromino
t =
    { shape = [ ( 0, 0 ), ( 1, 0 ), ( 2, 0 ), ( 1, 1 ) ]
    , block = Block Color.purple
    , pivot = ( 0, 0 )
    }
