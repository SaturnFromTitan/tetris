module Main exposing (main)

import Block
import Board exposing (Board, addTetromino, clearLines)
import Browser
import Browser.Events exposing (onAnimationFrameDelta)
import Collage.Layout exposing (horizontal, vertical)
import Collage.Render exposing (svg)
import Html exposing (Html, text)
import Keyboard exposing (Key(..), RawKey)
import Keyboard.Arrows exposing (Arrows)
import Random
import ScoreBoard exposing (Score, initialScore)
import Tetromino exposing (Tetromino, rotate)
import Upcoming exposing (toForm)


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


useIfValid : Model -> Model -> Model
useIfValid new current =
    if isValid new then
        new

    else
        current


tryKicks : List ( Int, Int ) -> Model -> Model -> Model
tryKicks shifts outOfBoundsModel current =
    case shifts of
        [] ->
            current

        s :: otherShifts ->
            let
                shifted =
                    Tetromino.shift s outOfBoundsModel.falling

                newModel =
                    { outOfBoundsModel | falling = shifted }
            in
            if isValid newModel then
                newModel

            else
                tryKicks otherShifts outOfBoundsModel current


wallKick : Model -> Model -> Model
wallKick outOfBoundsModel current =
    let
        range =
            outOfBoundsModel.falling.cols // 2

        shifts =
            List.range 1 range
                |> List.concatMap (\n -> [ ( n, 0 ), ( -n, 0 ) ])
    in
    tryKicks shifts outOfBoundsModel current


spawnTetromino : Model -> Model
spawnTetromino model =
    let
        -- the default case doesn't trigger as an empty bag is refreshed after
        -- taking one out. See below.
        newFalling =
            List.head model.bag
                |> Maybe.withDefault Tetromino.i
                |> Tetromino.shift startingShift

        newBag =
            List.drop 1 model.bag

        ( newBag_, seed_ ) =
            if List.isEmpty newBag then
                Random.step Tetromino.bag model.seed

            else
                ( newBag, model.seed )

        ( numClearedLines, newBoard ) =
            addTetromino model.falling model.board
                |> clearLines
    in
    { model
        | falling = newFalling
        , board = newBoard
        , seed = seed_
        , bag = newBag_
        , score = ScoreBoard.updateScore model.score numClearedLines
        , clearedLines = model.clearedLines + numClearedLines
    }


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

                newModel =
                    case arrowKey of
                        Just Keyboard.ArrowUp ->
                            let
                                rotated =
                                    { model
                                        | falling = model.falling |> rotate
                                    }

                                newModel_ =
                                    if isValid rotated then
                                        rotated

                                    else
                                        wallKick rotated model
                            in
                            newModel_

                        Just Keyboard.ArrowDown ->
                            { model | falling = model.falling |> Tetromino.shift ( 0, -1 ) }

                        Just Keyboard.ArrowLeft ->
                            { model | falling = model.falling |> Tetromino.shift ( -1, 0 ) }

                        Just Keyboard.ArrowRight ->
                            { model | falling = model.falling |> Tetromino.shift ( 1, 0 ) }

                        _ ->
                            model
            in
            ( useIfValid newModel model
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

        upcomingForm =
            List.head model.bag
                |> Maybe.withDefault Tetromino.i
                |> Upcoming.toForm sideBarWidth

        scoreBoardForm =
            model.score
                |> ScoreBoard.toForm sideBarWidth

        sideBarForm =
            vertical
                [ upcomingForm
                , scoreBoardForm
                ]
    in
    horizontal
        [ boardForm
        , sideBarForm
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
