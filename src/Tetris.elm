module Main exposing (main)

import Block
import Board exposing (..)
import Browser
import Browser.Events exposing (onAnimationFrameDelta)
import Collage.Layout exposing (..)
import Collage.Render exposing (svg)
import Html exposing (Html, text)
import Keyboard exposing (Key(..), RawKey)
import Keyboard.Arrows exposing (..)
import Random
import ScoreBoard exposing (..)
import Tetromino exposing (..)


type Msg
    = KeyDown RawKey
    | Frame Float


type alias Model =
    { seed : Random.Seed
    , bag : List Tetromino
    , board : Board
    , falling : Tetromino
    , clearedLines : Int
    , score : Score
    , time : Float
    , nextShift : Float
    , shiftDelay : Float
    }


initialSeed : Int
initialSeed =
    43


emptyBoard : Board
emptyBoard =
    Board.new []


startingShift : ( Int, Int )
startingShift =
    ( 5, 18 )


init : () -> ( Model, Cmd Msg )
init _ =
    let
        ( bag, newSeed ) =
            Random.step Tetromino.bag (Random.initialSeed initialSeed)

        -- TODO: Replace Maybe.withDefault
        falling =
            List.head bag |> Maybe.withDefault Tetromino.i

        newBag =
            List.drop 1 bag
    in
    ( { bag = newBag
      , seed = newSeed
      , board = emptyBoard
      , falling = falling |> Tetromino.shift startingShift
      , clearedLines = 0
      , score = initialScore
      , time = 0.0
      , nextShift = 1000.0
      , shiftDelay = 1000.0
      }
    , Cmd.none
    )


isValid : Model -> Bool
isValid model =
    Board.isValid model.falling model.board


spawnTetromino : Model -> Model
spawnTetromino model =
    let
        ( bag_, seed_ ) =
            if List.isEmpty model.bag then
                Random.step Tetromino.bag model.seed

            else
                ( model.bag, model.seed )

        -- TODO: Replace Maybe.withDefault
        newFalling =
            List.head bag_
                |> Maybe.withDefault Tetromino.i
                |> Tetromino.shift startingShift

        newBag =
            List.drop 1 bag_

        ( numClearedLines, newBoard ) =
            addTetromino model.falling model.board
                |> clearLines
    in
    { model
        | falling = newFalling
        , board = newBoard
        , seed = seed_
        , bag = newBag
        , score = ScoreBoard.updateScore model.score numClearedLines
        , clearedLines = model.clearedLines + numClearedLines
    }


useIfValid : Model -> Model -> Model
useIfValid new current =
    if isValid new then
        new

    else
        current


calculateShiftDelay : Int -> Float
calculateShiftDelay level =
    (-(sqrt (toFloat level) / sqrt 15) + 1) * 1000


checkTick : Model -> Model
checkTick model =
    if model.time < model.nextShift then
        model

    else
        { model
            | nextShift = model.time + model.shiftDelay
            , falling = model.falling |> Tetromino.shift ( 0, -1 )
        }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        KeyDown key ->
            let
                arrowKey =
                    Keyboard.Arrows.arrowKey key

                newFalling =
                    case arrowKey of
                        Just Keyboard.ArrowUp ->
                            model.falling |> rotate

                        Just Keyboard.ArrowDown ->
                            model.falling |> Tetromino.shift ( 0, -1 )

                        Just Keyboard.ArrowLeft ->
                            model.falling |> Tetromino.shift ( -1, 0 )

                        Just Keyboard.ArrowRight ->
                            model.falling |> Tetromino.shift ( 1, 0 )

                        _ ->
                            model.falling
            in
            ( useIfValid
                { model | falling = newFalling }
                model
            , Cmd.none
            )

        Frame delta ->
            let
                newTime =
                    model.time + delta

                modelWithNewTime =
                    { model | time = newTime }

                newModel_ =
                    checkTick modelWithNewTime

                newModel =
                    useIfValid newModel_ (spawnTetromino model)
            in
            ( useIfValid newModel model
            , Cmd.none
            )


view : Model -> Html msg
view model =
    let
        boardForm =
            model.board
                |> addTetromino model.falling
                |> Board.toForm

        sideBarWidth =
            6 * round Block.size

        scoreBoardForm =
            model.score
                |> ScoreBoard.toForm sideBarWidth
    in
    horizontal
        [ boardForm
        , scoreBoardForm
        ]
        |> svg


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Keyboard.downs KeyDown
        , onAnimationFrameDelta Frame
        ]


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }
