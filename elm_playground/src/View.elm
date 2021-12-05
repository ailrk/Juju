module View exposing (..)

import Html exposing (Html, button, div, h2, input, text)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import LeafletMap
import Types exposing (..)


view : Model -> Html Msg
view model =
    div
        []
        [ div []
            [ Html.text "Hello" ]
        , div []
            -- [ style "width" "80%"
            -- , style "height" "80%"
            -- ]
            [ input [ placeholder "Lat:", value (String.fromFloat model.lng) ] []
            , input [ placeholder "Lng:", value (String.fromFloat model.lat) ] []
            , button [ onClick Dec ] [ text "Find Route" ]
            , canvasMap
            ]
        ]


canvasMap : Html Msg
canvasMap =
    LeafletMap.view
        [ LeafletMap.mapId "canvasMapId"
        , LeafletMap.className "canvasMap"
        , LeafletMap.defaultPopup "map"
        , LeafletMap.iconHeight 64
        , LeafletMap.iconWidth 64
        , LeafletMap.iconUrl "https://image.flaticon.com/icons/svg/194/194648.svg"
        , LeafletMap.latitude 49.282
        , LeafletMap.longitude 123.12
        , LeafletMap.scale 13
        , LeafletMap.showDefaultMarker True
        , LeafletMap.showScale True
        , LeafletMap.tileLayer "https://{s}.tile.openstreetmap.org/{z}/{x}/{y}.png"
        ]
        [ ( "marker"
          , LeafletMap.marker
                [ LeafletMap.iconUrl "https://image.flaticon.com/icons/svg/194/194648.svg"
                , LeafletMap.iconHeight 64
                , LeafletMap.iconWidth 64
                , LeafletMap.latitude 49.282
                , LeafletMap.longitude 123.12
                ]
                []
          )
        ]



-- div []
--     [ button [ onClick Dec ] [ text "-" ]
--     , div [] [ text (String.fromInt model.num) ]
--     , button [ onClick Inc ] [ text "+" ]
--     , input [ placeholder "Text to reverse ", value model.msg, onInput Chg ] []
--     , div [] [ text (String.reverse model.msg) ]
--     ]
