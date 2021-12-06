module View exposing (..)

import Browser exposing (Document)
import Debug exposing (toString)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Regex exposing (..)
import Types exposing (..)


view : Model -> Document Msg
view model =
    { title = "Path finder"
    , body = [ body model ]
    }


body : Model -> Html Msg
body model =
    div []
        [ div [ class "col-md-2" ]
            [ div [ class "panel panel-info" ]
                [ div [ class "panel-heading" ] [ text "Contoller" ]
                , div [ class "panel-body" ]
                    [ h4 [] [ text "Coordinate Bound" ]
                    , div [] (List.map addBound model.bounds)
                    , div [ style "margin" "10px" ] []
                    , h4 [] [ text "Marker Type:  ", text (toString model.markerMode) ]
                    , button [ onClick ToggleMarkerType, class "btn btn-light", style "margin-right" "5px" ] [ text "Toggle Mode" ]
                    , br [] []
                    , br [] []
                    , h4 [] [ text "Map Control:  " ]
                    , button [ style "margin-right" "5px", onClick (ZoomMap 1), class "btn btn-light" ] [ text "Zoom In" ]
                    , button [ onClick (ZoomMap -1), class "btn btn-light" ] [ text "Zoom Out" ]
                    , addMarkers model
                    ]
                ]
            ]
        , div [ class "col-md-8" ]
            [ div [ class "panel panel-info" ]
                [ div [ class "panel-heading" ] [ text "State" ]
                , div [ class "panel-body" ]
                    [ pre [] [ text (toString model) ]
                    , div [ id "mapid" ] []
                    ]
                ]
            ]
        ]


addBound : Float -> Html Msg
addBound bound =
    div [] [ text (toString bound) ]



-- whether to add to sink or source depends on the current marker mode.


addMarkers : Model -> Html Msg
addMarkers model =
    case model.markerMode of
        Sink ->
            ul [ style "margin-top" "10px" ] << List.map addMarker <| model.sinks

        Source ->
            ul [ style "margin-top" "10px" ] << List.map addMarker <| model.sources



-- add a sinlge marker.


addMarker : Marker -> Html Msg
addMarker mk =
    let
        (Marker t m) =
            mk

        markerName =
            span
                [ case t of
                    Sink ->
                        style "color" "green"

                    Source ->
                        style "color" "red"
                ]
                [ text
                    (String.concat
                        [ case t of
                            Sink ->
                                "Sink"

                            Source ->
                                "Source"
                        , ": "
                        , toString m.lat
                        , ", "
                        , toString m.lng
                        ]
                    )
                ]
    in
    li [ style "martin-botom" "20px" ]
        [ button
            [ class "btn btn-ligth"
            , style "display" "inline-block"
            , style "margin-right" "5px"
            , onClick (RemoveMarker m.id)
            ]
            [ text "Remove" ]
        , markerName
        ]
