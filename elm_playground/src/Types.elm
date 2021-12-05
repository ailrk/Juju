module Types exposing (..)

type alias Model =
    { num : Int
    , msg : String
    , lng : Float
    , lat : Float
    }


type Msg
    = Inc
    | Dec
    | Chg String

    | FindPath
