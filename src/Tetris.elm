module Main exposing (main)

import Board exposing (..)
import Browser
import Collage.Render exposing (svg)
import Html exposing (Html, text)
import Keyboard exposing (Key(..))
import Keyboard.Arrows
import Random
import Tetromino exposing (..)


type Msg
    = KeyboardMsg Keyboard.Msg


{-| We don't need any other info in the model, since we can get everything we
need using the helpers right in the `view`!
This way we always have a single source of truth, and we don't need to remember
to do anything special in the update.
-}
type alias Model =
    { pressedKeys : List Key
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
    ( { pressedKeys = []
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
    let
        arrows =
            Keyboard.Arrows.arrows model.pressedKeys

        isDownShift =
            arrows.y == -1
    in
    isDownShift && not (isValid model)


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
        KeyboardMsg keyMsg ->
            let
                updatedKeys =
                    Keyboard.update keyMsg model.pressedKeys

                arrows =
                    Keyboard.Arrows.arrows updatedKeys

                newFalling =
                    if arrows.y == 1 then
                        model.falling |> rotate

                    else
                        model.falling |> Tetromino.shift ( arrows.x, arrows.y )
            in
            ( useIfValid
                model
                { model
                    | pressedKeys = updatedKeys
                    , falling = newFalling
                }
            , Cmd.none
            )


view : Model -> Html msg
view model =
    model.board |> addTetromino model.falling |> Board.toForm |> svg


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.map KeyboardMsg Keyboard.subscriptions


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }
