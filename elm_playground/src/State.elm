module State exposing (..)

import Browser.Navigation as Nav
import Html.Attributes exposing (..)
import String exposing (split)
import Subs exposing (removeMarker, zoomMap)
import Types exposing (..)
import Url


init : () -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init _ _ _ =
    ( { num = 0
      , msg = ""
      , sinks = []
      , sources = []
      , bounds = []
      }
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
            ( { model | sinks = marker :: model.sinks }, Cmd.none )

        RemoveMarker int ->
            ( { model | sinks = List.filter (\i -> i.id /= int) model.sinks }, removeMarker int )

        -- navigation
        UrlChanged _ ->
            ( model, Cmd.none )

        LinkClicked _ ->
            ( model, Cmd.none )
