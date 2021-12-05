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
        [ div [ class "col-md-4" ]
            [ div [ class "panel panel-info" ]
                [ div [ class "panel-heading" ] [ text "Contoller" ]
                , div [ class "panel-body" ]
                    [ h4 [] [ text "Bounds" ]
                    , div [] (List.map addBound model.bounds)
                    , button [ onClick (ZoomMap 1), class "btn btn-primary" ] [ text "Zoom In" ]
                    , div [ style "margin" "10px" ] []
                    , button [ onClick (ZoomMap -1), class "btn btn-primary" ] [ text "Zoom Out" ]
                    , addMarkers model.sinks
                    ]
                ]
            ]
        , div [ class "col-md-4" ]
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


addMarkers : List Marker -> Html Msg
addMarkers =
    ul [ style "margin-top" "10px" ] << List.map addMarker


addMarker : Marker -> Html Msg
addMarker mk =
    li [ style "martin-botom" "20px" ]
        [ button
            [ class "btn btn-warning"
            , style "display" "inline-block"
            , onClick (RemoveMarker mk.id)
            ]
            [ text "Remove" ]
        , span [] [ text ("Marker: " ++ toString mk.lat ++ ", " ++ toString mk.lng) ]
        ]
