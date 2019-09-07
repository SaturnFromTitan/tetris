module Main exposing (main)

import Board exposing (..)
import Browser
import Collage.Render exposing (svg)
import Html exposing (Html, text)
import Keyboard exposing (Key(..))
import Keyboard.Arrows
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
    , board : Board
    , falling : Tetromino
    }


emptyBoard : Board
emptyBoard =
    Board.new []


init : () -> ( Model, Cmd Msg )
init _ =
    ( { pressedKeys = []
      , board = emptyBoard
      , falling = j
      }
    , Cmd.none
    )


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
            ( { model
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
