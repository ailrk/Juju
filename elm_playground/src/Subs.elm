port module Subs exposing (..)

import Types exposing (..)


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch [ setBounds SetBounds, addMarker AddMarker ]

port setBounds : (String -> msg) -> Sub msg
port zoomMap : Int -> Cmd msg
port addMarker : (Marker -> msg) -> Sub msg
port removeMarker : Int -> Cmd msg
