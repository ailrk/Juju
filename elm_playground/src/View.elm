module View exposing (..)

import Browser exposing (Document)
import Debug exposing (toString)
import Html exposing (Html, button, div, h4, input, li, span, text, ul)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
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
                [ div [ class "panel-heading" ] [ text "Path finding" ]
                , div
                    [ class "panel-body"
                    , style "width" "80%"
                    , style "height" "80%"
                    ]
                    [ h4 [] [ text "Bounds" ] -- TODO map here
                    ]
                ]
            ]
        , div [ class "col-md-4" ]
            [ h4 [] [ text "Body" ] ]
        ]


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
