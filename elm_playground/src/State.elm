module State exposing (..)

import Html.Attributes exposing (..)
import Types exposing (..)


init : () -> ( Model, Cmd Msg )
init _ =
    ( { num = 0
      , msg = ""
      , lng = 0
      , lat = 0
      }
    , Cmd.none
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Inc ->
            ( { model | num = model.num + 1 }, Cmd.none )

        Dec ->
            ( { model | num = model.num - 1 }, Cmd.none )

        Chg newMsg ->
            ( { model | msg = newMsg }, Cmd.none )

        -- find path
        FindPath ->
            ( model, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none
