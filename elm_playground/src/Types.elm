module Types exposing (..)

import Browser
import Url


type alias Model =
    { markerMode : MarkerType
    , sinks : List Marker -- sinks of the path find algorithm
    , sources : List Marker -- sources of the path find algorithm.
    , bounds : List Float
    , paths : List Path
    }



-- A marker is a unique (lat, lng) pair that you can pin on the map.
-- Raw marker stores js compatible location information. It should be used
-- for the communication with ffi.


type alias RawMarker =
    { id : Int
    , lat : Float
    , lng : Float
    }



-- wrapper on top of RawMarker with the type of the marker.


type Marker
    = Marker MarkerType RawMarker



-- a source is where the path finding starts, a source is where path finding
-- ends.


type MarkerType
    = Sink
    | Source



-- path bettween two markerse. Only source and since can form a path.
-- TODO


type alias Path =
    { from : RawMarker
    , to : RawMarker
    , time : Float
    , distance : Float
    }


pathid : Path -> Int
pathid { from, to } =
    from.id + to.id


type Msg
    = -- map control
      SetBounds String
    | ZoomMap Int
    | AddMarker Marker
    | RemoveMarker Int
    | ToggleMarkerType
    | Roll
    | GenerateRandomMarkers ( MarkerType, Float, Float )
    | FindPath ( Marker, Marker )
    | AddPath Path
    | FindPathAll
    | Clear
      -- navigation
    | LinkClicked Browser.UrlRequest
    | UrlChanged Url.Url
