module Tetris exposing (Model, Msg(..), init, main, subscriptions, update, view)

import Browser
import Browser.Events exposing (onAnimationFrameDelta)
import Html exposing (text)


type alias Model =
    { count : Float }


init _ =
    ( { count = 0 }, Cmd.none )


type Msg
    = Frame Float


subscriptions _ =
    onAnimationFrameDelta Frame


update _ model =
    ( { model | count = model.count + 1 }, Cmd.none )


view model =
    text ("Count: " ++ String.fromFloat model.count)


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
