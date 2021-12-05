module Types exposing (..)

import Browser
import Url


type alias Model =
    { num : Int
    , msg : String
    , sinks : List Marker -- sinks of the path find algorithm
    , sources : List Marker -- sources of the path find algorithm.
    , bounds : List Float
    }


type alias Marker =
    { id : Int
    , lat : Float
    , lng : Float
    }


type Msg
    = -- map control
      SetBounds String
    | ZoomMap Int
    | AddMarker Marker
    | RemoveMarker Int
      -- navigation
    | LinkClicked Browser.UrlRequest
    | UrlChanged Url.Url
