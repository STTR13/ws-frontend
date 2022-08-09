module Pages.LogIn exposing (Model, Msg, page)

import API.Object.Error
import API.Object.Token
import API.Query
import API.Union.LoginRes
import Color
import Effect exposing (Effect)
import Element exposing (Element)
import Element.Background
import Element.Border
import Element.Font
import Element.Input
import Email exposing (Email)
import Form exposing (Form)
import Graphql.Http
import Graphql.Http.GraphqlError
import Graphql.SelectionSet
import Html.Attributes
import Html.Events
import Json.Decode as Decode
import Material.Icons
import Material.Icons.Types
import Page
import Request exposing (Request)
import Shared
import Svg
import Svg.Attributes
import Tuple.Extra
import View exposing (View)


type alias Model =
    { formState : FormState
    , errorState : Maybe Error
    }


type Error
    = ServerError { code : Int, message : String }
    | HttpError Graphql.Http.HttpError
    | GraphqlError (List Graphql.Http.GraphqlError.GraphqlError)


type Msg
    = FormUpdate FormMsg
    | LogInSuccess Shared.User
    | LogInError Error
    | CloseError


init : ( Model, Effect Msg )
init =
    ( { formState = initFormState
      , errorState = Nothing
      }
    , Effect.none
    )


update : Msg -> Model -> ( Model, Effect Msg )
update msg model =
    case msg of
        FormUpdate formMsg ->
            let
                ( newFormState, maybeOutput ) =
                    form.update formMsg model.formState
                        |> Debug.log "fromOut"

                effect =
                    case maybeOutput of
                        Just output ->
                            Effect.fromCmd <| toQuery output

                        Nothing ->
                            Effect.none
            in
            { model | formState = newFormState }
                |> Tuple.Extra.pairWith effect

        LogInSuccess user ->
            { model | errorState = Nothing }
                |> Tuple.Extra.pairWith (Effect.fromShared <| Shared.LogIn user)

        LogInError error ->
            { model | errorState = Just error }
                |> Tuple.Extra.pairWith Effect.none

        CloseError ->
            { model | errorState = Nothing }
                |> Tuple.Extra.pairWith Effect.none


view : Model -> View Msg
view model =
    let
        fillAttr =
            [ Element.width Element.fill
            , Element.height Element.fill
            ]

        errorPopup =
            case model.errorState of
                Nothing ->
                    Element.none

                Just error ->
                    Element.row
                        [ Element.width Element.fill
                        , Element.Background.color (Element.rgb 1 0 0)
                        ]
                        [ Element.el
                            [ Element.centerY
                            , Element.Font.color (Element.rgb 1 1 1)
                            ]
                            (errorToText error |> Element.text)
                        , Element.Input.button
                            [ Element.Background.color (Element.rgb 0.95 0 0)
                            , Element.Border.rounded 3
                            ]
                            { onPress = Just CloseError
                            , label =
                                Material.Icons.close 10
                                    (Material.Icons.Types.Color <| Color.rgb 1 1 1)
                                    |> List.singleton
                                    |> Svg.svg [ Svg.Attributes.width "10", Svg.Attributes.height "10", Svg.Attributes.viewBox "0 0 10 10" ]
                                    |> Element.html
                            }
                        ]
    in
    { title = "sign in"
    , body =
        Element.column fillAttr
            [ errorPopup
            , Element.column
                [ Element.centerX
                , Element.centerY
                , Element.spacing 10
                ]
                (form.view model.formState)
                |> Element.map FormUpdate
                |> Element.el fillAttr
            ]
    }


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


page : Shared.Model -> Request -> Page.With Model Msg
page _ _ =
    Page.advanced
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }



-- ███████╗ ██████╗ ██████╗ ███╗   ███╗
-- ██╔════╝██╔═══██╗██╔══██╗████╗ ████║
-- █████╗  ██║   ██║██████╔╝██╔████╔██║
-- ██╔══╝  ██║   ██║██╔══██╗██║╚██╔╝██║
-- ██║     ╚██████╔╝██║  ██║██║ ╚═╝ ██║
-- ╚═╝      ╚═════╝ ╚═╝  ╚═╝╚═╝     ╚═╝


type alias FormState =
    { email : Form.FieldState String String
    , password : Form.FieldState Never String
    }


initFormState =
    { email = { value = "", error = Nothing }
    , password = { value = "", error = Nothing }
    }


type FormMsg
    = TypeEmail String
    | TypePassword String
    | Submit


type alias FormOutput =
    { email : Email
    , password : String
    }


form : Form FormState FormMsg FormOutput
form =
    let
        emailField : Form.Field String String Email
        emailField =
            { parse = Email.fromString >> Result.fromMaybe "invalid email"
            , transform = identity
            , view =
                \{ value, error } ->
                    Element.Input.email
                        [ if error == Nothing then
                            noneAttribute

                          else
                            Element.Border.color (Element.rgb 1 0 0)
                        ]
                        { onChange = identity
                        , text = value
                        , placeholder = Element.Input.placeholder [] (Element.text "your@email.com") |> Just
                        , label = Element.Input.labelAbove [] (Element.text "Email")
                        }
            }

        emailInterface : Form.Interface FormState FormMsg String String
        emailInterface =
            { fieldState =
                { get = .email
                , insert = \fieldState formState -> { formState | email = fieldState }
                }
            , msg =
                { fromValue = TypeEmail
                , toValue =
                    \msg ->
                        case msg of
                            TypeEmail val ->
                                Just val

                            _ ->
                                Nothing
                }
            }

        passwordField : Form.Field Never String String
        passwordField =
            { parse = Ok
            , transform = identity
            , view =
                \{ value } ->
                    Element.Input.currentPassword
                        []
                        { onChange = identity
                        , text = value
                        , placeholder = Element.Input.placeholder [] (Element.text "yourPassword") |> Just
                        , label = Element.Input.labelAbove [] (Element.text "Password")
                        , show = False
                        }
            }

        passwordInterface : Form.Interface FormState FormMsg Never String
        passwordInterface =
            { fieldState = { get = .password, insert = \fieldState formState -> { formState | password = fieldState } }
            , msg =
                { fromValue = TypePassword
                , toValue =
                    \msg ->
                        case msg of
                            TypePassword val ->
                                Just val

                            _ ->
                                Nothing
                }
            }

        submitButton =
            { isSubmit = (==) Submit
            , view =
                Element.Input.button
                    [ Element.alignRight
                    , Element.padding 7
                    , Element.Background.color (Element.rgb 0.85 0 0)
                    , Element.Font.color (Element.rgb 1 1 1)
                    , Element.Font.bold
                    , Element.Border.rounded 3
                    ]
                    { onPress = Just Submit
                    , label = Element.text "Submit"
                    }
            }
    in
    Form.succeed FormOutput
        |> Form.append (Form.toForm emailInterface emailField)
        |> Form.append (Form.toForm passwordInterface passwordField)
        |> Form.appendSubmitButton submitButton



--  █████╗ ██████╗ ██╗
-- ██╔══██╗██╔══██╗██║
-- ███████║██████╔╝██║
-- ██╔══██║██╔═══╝ ██║
-- ██║  ██║██║     ██║
-- ╚═╝  ╚═╝╚═╝     ╚═╝


toQuery : FormOutput -> Cmd Msg
toQuery { email, password } =
    let
        handleHttpErrors : Result (Graphql.Http.Error Msg) Msg -> Msg
        handleHttpErrors result =
            case result of
                Ok msg ->
                    msg

                Err (Graphql.Http.HttpError error) ->
                    HttpError error |> LogInError

                Err (Graphql.Http.GraphqlError _ errors) ->
                    GraphqlError errors |> LogInError
    in
    API.Query.login
        { id = Email.toString email, pwd = password }
        (API.Union.LoginRes.fragments
            { onToken =
                Graphql.SelectionSet.map
                    (\token ->
                        LogInSuccess { email = email, token = token }
                    )
                    API.Object.Token.token
            , onError =
                Graphql.SelectionSet.map2
                    (\code message ->
                        ServerError { code = code, message = message }
                            |> LogInError
                    )
                    API.Object.Error.code
                    API.Object.Error.message
            }
        )
        |> Graphql.Http.queryRequest Shared.apiUrl
        |> Graphql.Http.send handleHttpErrors



-- ██╗   ██╗████████╗██╗██╗
-- ██║   ██║╚══██╔══╝██║██║
-- ██║   ██║   ██║   ██║██║
-- ██║   ██║   ██║   ██║██║
-- ╚██████╔╝   ██║   ██║███████╗
--  ╚═════╝    ╚═╝   ╚═╝╚══════╝


errorToText : Error -> String
errorToText error =
    case error of
        ServerError { message } ->
            message

        GraphqlError _ ->
            "There is an issue in the program (graphql).\nPlease post an issue on github"

        HttpError httpErr ->
            case httpErr of
                Graphql.Http.Timeout ->
                    "The communication with the server timed out.\nPlease make sure your internet connection is working properly"

                Graphql.Http.NetworkError ->
                    "There was an issue when we tried reaching to the server.\nPlease make sure your internet connection is working properly"

                _ ->
                    "There is an issue in the program (http).\nPlease post an issue on github"


noneAttribute : Element.Attribute msg
noneAttribute =
    Element.htmlAttribute (Html.Attributes.classList [])


onEnter : msg -> Element.Attribute msg
onEnter msg =
    Element.htmlAttribute
        (Html.Events.on "keyup"
            (Decode.field "key" Decode.string
                |> Decode.andThen
                    (\key ->
                        if key == "Enter" then
                            Decode.succeed msg

                        else
                            Decode.fail "Not the enter key"
                    )
            )
        )
