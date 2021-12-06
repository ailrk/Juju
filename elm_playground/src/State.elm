module State exposing (..)

import Browser.Navigation as Nav
import Html.Attributes exposing (..)
import String exposing (split)
import Subs exposing (removeMarker, toggleMode_, zoomMap, clearAll)
import Types exposing (..)
import Url


initModel : Model
initModel =
    { markerMode = Sink
    , sinks = []
    , sources = []
    , bounds = []
    }


init : () -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init _ _ _ =
    ( initModel
    , Cmd.none
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SetBounds bounds ->
            ( { model | bounds = List.map (Maybe.withDefault 0 << String.toFloat) (split "," bounds) }, Cmd.none )

        ZoomMap int ->
            ( model, zoomMap int )

        AddMarker marker ->
            case marker of
                Marker Sink _ ->
                    ( { model | sinks = marker :: model.sinks }, Cmd.none )

                Marker Source _ ->
                    ( { model | sources = marker :: model.sources }, Cmd.none )

        ToggleMarkerType ->
            case model.markerMode of
                Sink ->
                    ( { model | markerMode = Source }, toggleMode_ Source )

                Source ->
                    ( { model | markerMode = Sink }, toggleMode_ Sink )

        RemoveMarker int ->
            case model.markerMode of
                Sink ->
                    ( { model | sinks = List.filter (\(Marker _ i) -> i.id /= int) model.sinks }, removeMarker int )

                Source ->
                    ( { model | sources = List.filter (\(Marker _ i) -> i.id /= int) model.sources }, removeMarker int )

        Clear ->
            ( initModel, clearAll () )

        -- navigation
        UrlChanged _ ->
            ( model, Cmd.none )

        LinkClicked _ ->
            ( model, Cmd.none )
