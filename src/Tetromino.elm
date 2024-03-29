module Tetromino exposing (Location, Point, Tetromino, bag, i, j, l, o, rotate, rotateLocation, s, shift, t, toForm, toLocation, toPoint, z)

import Block exposing (Block)
import Collage exposing (Collage, group)
import Collage.Render exposing (svg)
import Color exposing (Color)
import Html exposing (Html, text)
import List
import Random
import Random.List
import Tuple exposing (first, second)



-- Type aliases


type alias Location =
    ( Int, Int )


type alias Point =
    ( Float, Float )


type alias Tetromino =
    { shape : List Location
    , block : Block
    , pivot : Point
    , rows : Int
    , cols : Int
    }



-- converters


toPoint : Location -> Point
toPoint ( r, c ) =
    ( toFloat r * Block.size, toFloat c * Block.size )


toLocation : Point -> Location
toLocation ( r, c ) =
    ( round r, round c )


toForm : Tetromino -> Collage msg
toForm { shape, block } =
    let
        form =
            Block.toForm block

        translate location =
            Collage.shift
                (toPoint location)
                form

        forms =
            List.map translate shape
    in
    group forms



-- constants


i : Tetromino
i =
    { shape = [ ( 0, 1 ), ( 0, 0 ), ( 0, -1 ), ( 0, -2 ) ]
    , block = Block Color.lightBlue
    , pivot = ( 0.5, -0.5 )
    , cols = 1
    , rows = 4
    }


j : Tetromino
j =
    { shape = [ ( 0, 1 ), ( 0, 0 ), ( 0, -1 ), ( -1, -1 ) ]
    , block = Block Color.darkBlue
    , pivot = ( 0, 0 )
    , cols = 2
    , rows = 3
    }


l : Tetromino
l =
    { shape = [ ( 0, 1 ), ( 0, 0 ), ( 0, -1 ), ( 1, -1 ) ]
    , block = Block Color.orange
    , pivot = ( 0, 0 )
    , cols = 2
    , rows = 3
    }


o : Tetromino
o =
    { shape = [ ( 0, 1 ), ( 0, 0 ), ( 1, 0 ), ( 1, 1 ) ]
    , block = Block Color.yellow
    , pivot = ( 0.5, 0.5 )
    , cols = 2
    , rows = 2
    }


s : Tetromino
s =
    { shape = [ ( 0, 0 ), ( 1, 0 ), ( 1, 1 ), ( 2, 1 ) ]
    , block = Block Color.lightGreen
    , pivot = ( 1, 0 )
    , cols = 3
    , rows = 2
    }


z : Tetromino
z =
    { shape = [ ( 0, 1 ), ( 1, 1 ), ( 1, 0 ), ( 2, 0 ) ]
    , block = Block Color.red
    , pivot = ( 1, 0 )
    , cols = 3
    , rows = 2
    }


t : Tetromino
t =
    { shape = [ ( 0, 0 ), ( 1, 0 ), ( 2, 0 ), ( 1, 1 ) ]
    , block = Block Color.purple
    , pivot = ( 1, 0 )
    , cols = 3
    , rows = 2
    }



-- shuffled bag


tetrominoes : List Tetromino
tetrominoes =
    [ i, j, l, o, s, z, t ]


bag : Random.Generator (List Tetromino)
bag =
    Random.List.shuffle tetrominoes



-- rotation logic


rotateLocation : Point -> Float -> Location -> Location
rotateLocation ( pivotX, pivotY ) angle ( row, col ) =
    let
        rowOrigin =
            toFloat row - pivotX

        colOrigin =
            toFloat col - pivotY

        ( sinValue, cosValue ) =
            ( sin angle, cos angle )

        rowRotated =
            rowOrigin * cosValue - colOrigin * sinValue

        colRotated =
            rowOrigin * sinValue + colOrigin * cosValue
    in
    toLocation ( rowRotated + pivotX, colRotated + pivotY )


rotate : Tetromino -> Tetromino
rotate tetromino =
    let
        rotateAroundPivot =
            rotateLocation tetromino.pivot (degrees 90)

        newShape =
            List.map rotateAroundPivot tetromino.shape
    in
    { tetromino
        | shape = newShape
        , rows = tetromino.cols
        , cols = tetromino.rows
    }


shift : ( Int, Int ) -> Tetromino -> Tetromino
shift ( shiftX, shiftY ) tetromino =
    let
        shiftLocation ( row, col ) =
            ( row + shiftX, col + shiftY )

        newShape =
            List.map shiftLocation tetromino.shape

        newPivot =
            ( first tetromino.pivot + toFloat shiftX
            , second tetromino.pivot + toFloat shiftY
            )
    in
    { tetromino
        | shape = newShape
        , pivot = newPivot
    }
