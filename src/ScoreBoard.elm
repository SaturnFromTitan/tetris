module ScoreBoard exposing (Score, boxWithText, initialScore, toForm, updateScore)

import Collage exposing (..)
import Collage.Layout exposing (..)
import Collage.Render exposing (svg)
import Collage.Text exposing (..)
import Color exposing (Color)
import Html exposing (Html, text)
import String


type alias Score =
    { points : Int
    , lines : Int
    }


initialScore : Score
initialScore =
    { points = 0
    , lines = 0
    }


updateScore : Score -> Int -> Score
updateScore { points, lines } clearedLines =
    let
        -- Taken from https://tetris.wiki/Scoring#Original_Nintendo_scoring_system
        additionalPoints =
            case clearedLines of
                1 ->
                    40

                2 ->
                    100

                3 ->
                    300

                4 ->
                    1200

                _ ->
                    0
    in
    { lines = lines + clearedLines
    , points = points + additionalPoints
    }


boxWithText : Int -> String -> Int -> Collage msg
boxWithText width label value =
    let
        boxShapeWidth =
            toFloat width - 10

        boxShape =
            rectangle boxShapeWidth 45

        lineStyle =
            solid 3.0 (uniform Color.black)

        lineStyle_ =
            { lineStyle | cap = Round }

        border =
            outlined lineStyle_ boxShape

        filledBoxShape =
            boxShape |> filled (uniform Color.white)

        formatText s =
            fromString s
                |> typeface Monospace
                |> size large
                |> weight Bold
                |> rendered
    in
    group
        [ border
        , (label ++ ":")
            |> formatText
            |> shift ( 0, 12 )
        , value
            |> String.fromInt
            |> formatText
            |> shift ( 0, -8 )
        , filledBoxShape
        ]


toForm : Int -> Score -> Collage msg
toForm width score =
    vertical
        [ boxWithText width "Points" score.points
        , boxWithText width "Lines" score.lines
        ]
