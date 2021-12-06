module State exposing (..)

import Browser.Navigation as Nav
import Compare exposing (..)
import Html.Attributes exposing (..)
import Random
import String exposing (split)
import Subs exposing (clearAll, findPath, pushMarker, removeMarker, toggleMode, zoomMap)
import Types exposing (..)
import Url


initModel : Model
initModel =
    { markerMode = Sink
    , sinks = []
    , sources = []
    , bounds = []
    , paths = []
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
            ( { model
                | bounds =
                    List.map (Maybe.withDefault 0 << String.toFloat)
                        (split "," bounds)
              }
            , Cmd.none
            )

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
                    ( { model | markerMode = Source }, toggleMode Source )

                Source ->
                    ( { model | markerMode = Sink }, toggleMode Sink )

        RemoveMarker int ->
            case model.markerMode of
                Sink ->
                    ( { model
                        | sinks =
                            List.filter (\(Marker _ i) -> i.id /= int)
                                model.sinks
                      }
                    , removeMarker int
                    )

                Source ->
                    ( { model
                        | sources =
                            List.filter (\(Marker _ i) -> i.id /= int)
                                model.sources
                      }
                    , removeMarker int
                    )

        -- roll a  random (MarkerType, Float, Float).
        Roll ->
            ( model
            , Random.generate
                GenerateRandomMarkers
                (randomMarker ( 49.18770933925339, -123.17818043177067 )
                    ( 49.11741959452222, -122.90737245428662 )
                )
            )

        -- pushMaker will trigger a AddMarker at the other side.
        GenerateRandomMarkers tup ->
            ( model, pushMarker tup )

        -- find the shortest path between two points.
        FindPath tup ->
            case tup of
                ( Marker Source rm1, Marker Sink rm2 ) ->
                    ( model, findPath ( rm1, rm2 ) )

                ( Marker Sink rm1, Marker Source rm2 ) ->
                    ( model, findPath ( rm1, rm2 ) )

                _ ->
                    ( model, Cmd.none )

        AddPath path ->
            ( { model | paths = path :: model.paths }, Cmd.none )

        -- before: path is empty
        -- after: 1. each sink found a cloesest warehouse.
        --        2. their path in paths
        FindPathAll ->
            ( model, scanAll model.sinks model.sources )

        Clear ->
            ( initModel, clearAll () )

        -- navigation
        UrlChanged _ ->
            ( model, Cmd.none )

        LinkClicked _ ->
            ( model, Cmd.none )



--  take a sink, and a list  of source, zip with each one,
--  for each pair [ (s, S1), (s, S2) ..], run findPath to get path in paths
--
-- scan all source and
-- for all [x x x x x]


scanAll : List Marker -> List Marker -> Cmd Msg
scanAll xs ys =
    let
        f ms1 ms2 =
            case ms1 of
                m :: ms ->
                    scan1 m ms2 :: f ms ms2

                [] ->
                    []
    in
    Cmd.batch (f xs ys)


scan1 : Marker -> List Marker -> Cmd Msg
scan1 marker ls =
    let
        (Marker _ rm1) =
            marker

        zipped =
            List.map (\(Marker _ rm2) -> ( rm1, rm2 )) ls
    in
    Cmd.batch (List.map findPath zipped)



-- [ p1, p2, ... ]


minPath : List Path -> Maybe Path
minPath =
    List.head << List.sortBy .time



-- random marker


randomMarkerType : Random.Generator MarkerType
randomMarkerType =
    Random.uniform Sink [ Source ]


randomLatlng : ( Float, Float ) -> ( Float, Float ) -> Random.Generator ( Float, Float )
randomLatlng ( x1, y1 ) ( x2, y2 ) =
    Random.float x1 x2
        |> Random.andThen
            (\xrand -> Random.map (\yrand -> ( xrand, yrand )) (Random.float y1 y2))


randomMarker : ( Float, Float ) -> ( Float, Float ) -> Random.Generator ( MarkerType, Float, Float )
randomMarker p1 p2 =
    randomMarkerType
        |> Random.andThen
            (\t ->
                randomLatlng p1 p2
                    |> Random.andThen (\( x, y ) -> Random.constant ( t, x, y ))
            )
