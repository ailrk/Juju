port module Subs exposing (..)

import Types exposing (..)


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ setBounds SetBounds
        , addMarker (\rawMarker -> Marker model.markerMode rawMarker |> AddMarker)
        , toggleModeJs (\_ -> ToggleMarkerType)
        , addPath AddPath
        ]



-- subscribe the latlng bound change.


port setBounds : (String -> msg) -> Sub msg



-- change zoom level


port zoomMap : Int -> Cmd msg



-- subscribe to the clicking event on map.


port addMarker : (RawMarker -> msg) -> Sub msg



-- actively push new markers to the model. Note it's a cmd.


port pushMarker_ : ( String, Float, Float ) -> Cmd msg


pushMarker : ( MarkerType, Float, Float ) -> Cmd msg
pushMarker tup =
    case tup of
        ( Sink, x, y ) ->
            pushMarker_ ( "sink", x, y )

        ( Source, x, y ) ->
            pushMarker_ ( "source", x, y )



-- remove marker by id.


port removeMarker : Int -> Cmd msg



-- add path to model.


port addPath : (Path -> msg) -> Sub msg



-- find path. The result will be returned from js by sending to addPath.


port findPath : ( RawMarker, RawMarker ) -> Cmd msg


port clearAll : () -> Cmd msg



-- toggle the current marker type.


port toggleMode_ : String -> Cmd msg


toggleMode : MarkerType -> Cmd msg
toggleMode t =
    case t of
        Sink ->
            toggleMode_ "sink"

        Source ->
            toggleMode_ "source"



-- toggle from js


port toggleModeJs : (() -> msg) -> Sub msg
