port module Subs exposing (..)

import Types exposing (..)


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch [ setBounds SetBounds, addMarker (\rawMarker -> Marker model.markerMode rawMarker |> AddMarker) ]


port setBounds : (String -> msg) -> Sub msg


port zoomMap : Int -> Cmd msg


port addMarker : (RawMarker -> msg) -> Sub msg


port removeMarker : Int -> Cmd msg
