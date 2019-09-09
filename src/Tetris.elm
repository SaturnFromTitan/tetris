module Main exposing (main)

import Board exposing (..)
import Browser
import Collage.Render exposing (svg)
import Html exposing (Html, text)
import Keyboard exposing (Key(..), RawKey)
import Keyboard.Arrows exposing (..)
import Random
import Tetromino exposing (..)


type Msg
    = KeyDown RawKey


type alias Model =
    { lastKey : Maybe Key
    , seed : Random.Seed
    , bag : List Tetromino
    , board : Board
    , falling : Tetromino
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
    ( { lastKey = Nothing
      , bag = newBag
      , seed = newSeed
      , board = emptyBoard
      , falling = falling |> Tetromino.shift startingShift
      }
    , Cmd.none
    )


isValid : Model -> Bool
isValid model =
    Board.isValid model.falling model.board


needSpawn : Model -> Bool
needSpawn model =
    model.lastKey == Just Keyboard.ArrowDown && not (isValid model)


useIfValid : Model -> Model -> Model
useIfValid current new =
    if isValid new then
        new

    else if needSpawn new then
        spawnTetromino current

    else
        current


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

        newBoard =
            addTetromino model.falling model.board
    in
    { model
        | falling = newFalling
        , board = newBoard
        , seed = seed_
        , bag = newBag
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
                model
                { model | falling = newFalling, lastKey = arrowKey }
            , Cmd.none
            )


view : Model -> Html msg
view model =
    model.board |> addTetromino model.falling |> Board.toForm |> svg


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Keyboard.downs KeyDown
        ]


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }
