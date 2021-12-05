module Main exposing (main)

import Browser
import State exposing (..)
import Types exposing (..)
import View exposing (..)


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
