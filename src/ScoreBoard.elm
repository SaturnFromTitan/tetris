module ScoreBoard exposing (Score, boxWithText, initialScore, toForm, updateScore)

import Collage exposing (Collage, LineCap(..), filled, group, outlined, rectangle, rendered, shift, solid, uniform)
import Collage.Layout exposing (vertical)
import Collage.Render exposing (svg)
import Collage.Text exposing (Typeface(..), Weight(..), fromString, large, size, typeface, weight)
import Color exposing (Color)
import Html exposing (Html, text)
import String


type alias Score =
    { points : Int
    , lines : Int
    , level : Int
    }


initialScore : Score
initialScore =
    { points = 0
    , lines = 0
    , level = 0
    }


getAdditionalPoints : Int -> Int -> Int
getAdditionalPoints level clearedLines =
    let
        -- Taken from https://tetris.wiki/Scoring#Original_Nintendo_scoring_system
        clearedLinesFactor =
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

        levelFactor =
            level + 1
    in
    clearedLinesFactor * levelFactor


updateScore : Score -> Int -> Score
updateScore score clearedLines =
    let
        additionalPoints =
            getAdditionalPoints score.level clearedLines

        newPoints =
            score.points + additionalPoints

        newLines =
            score.lines + clearedLines

        newLevel =
            newLines // 10
    in
    { lines = newLines
    , points = newPoints
    , level = newLevel
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
        [ boxWithText width "Level" score.level
        , boxWithText width "Points" score.points
        , boxWithText width "Lines" score.lines
        ]
