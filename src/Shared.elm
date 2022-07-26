module Shared exposing (..)

import Email exposing (Email)
import Json.Decode as Json
import Request exposing (Request)


type alias Flags =
    Json.Value


{-| TODO give through flags
-}
apiUrl : String
apiUrl =
    "http://54.77.14.151:8080/query"


type alias Model =
    { user : Maybe User }


type alias User =
    { email : Email
    , token : String
    }


type Msg
    = LogIn User
    | LogOut


init : Request -> Flags -> ( Model, Cmd Msg )
init _ _ =
    ( { user = Nothing }, Cmd.none )


update : Request -> Msg -> Model -> ( Model, Cmd Msg )
update _ msg model =
    case msg of
        LogIn user ->
            ( { model | user = Just user }, Cmd.none )

        LogOut ->
            ( { model | user = Nothing }, Cmd.none )


subscriptions : Request -> Model -> Sub Msg
subscriptions _ _ =
    Sub.none
