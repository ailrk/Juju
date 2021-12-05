module State exposing (..)

import Browser.Navigation as Nav
import Html.Attributes exposing (..)
import Types exposing (..)
import Url


init : () -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init _ _ _ =
    ( { num = 0
      , msg = ""
      , sinks = []
      , sources = []
      }
    , Cmd.none
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        -- Inc ->
        --     ( { model | num = model.num + 1 }, Cmd.none )
        -- Dec ->
        --     ( { model | num = model.num - 1 }, Cmd.none )
        -- Chg newMsg ->
        --     ( { model | msg = newMsg }, Cmd.none )
        -- map control
        SetBounds _ ->
            ( model, Cmd.none )

        ZoomMap _ ->
            ( model, Cmd.none )

        AddMarker _ ->
            ( model, Cmd.none )

        RemoveMarker _ ->
            ( model, Cmd.none )

        -- navigation
        UrlChanged _ ->
            ( model, Cmd.none )

        LinkClicked _ ->
            ( model, Cmd.none )



-- -- find path
-- FindPath ->
--     ( model, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none
