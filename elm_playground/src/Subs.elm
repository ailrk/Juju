port module Subs exposing (..)

import Types exposing (..)


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch [ setBounds SetBounds, addMarker (\rawMarker -> Marker model.markerMode rawMarker |> AddMarker) ]


port setBounds : (String -> msg) -> Sub msg


port zoomMap : Int -> Cmd msg


port addMarker : (RawMarker -> msg) -> Sub msg


port removeMarker : Int -> Cmd msg


port clearAll : () -> Cmd msg

-- toggle the current marker type.


port toggleMode : String -> Cmd msg


toggleMode_ : MarkerType -> Cmd msg
toggleMode_ t =
    case t of
        Sink ->
            toggleMode "sink"

        Source ->
            toggleMode "source"
