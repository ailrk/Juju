module Main exposing (main)

import Browser
import State exposing (..)
import Types exposing (..)
import View exposing (..)


main : Program () Model Msg
main = Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        , onUrlChange = UrlChanged
        , onUrlRequest = LinkClicked
        }
