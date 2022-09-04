module Pages.LogIn exposing (Model, Msg, page)

import API.Object.Error
import API.Object.Token
import API.Query
import API.Union.LoginRes
import APIError
import Color
import Effect exposing (Effect)
import Element
import Element.Background
import Element.Border
import Element.Font
import Element.Input
import Email exposing (Email)
import Form exposing (Form)
import Form.CurrentPassword
import Form.Email
import Graphql.Http
import Graphql.SelectionSet
import Material.Icons
import Material.Icons.Types
import Page
import Request exposing (Request)
import Shared
import Svg
import Svg.Attributes
import Tuple.Extra
import View exposing (View)
import Void exposing (..)


type alias Model =
    { formState : FormState
    , formOutput : Maybe FormOutput
    , errorState : Maybe APIError.Error
    }


type Msg
    = FormUpdate FormMsg
    | LogInSuccess Shared.User
    | LogInError APIError.Error
    | CloseError


init : ( Model, Effect Msg )
init =
    ( { formState = initFormState
      , formOutput = Nothing
      , errorState = Nothing
      }
    , Effect.none
    )


update : Msg -> Model -> ( Model, Effect Msg )
update msg model =
    case msg of
        FormUpdate formMsg ->
            let
                form =
                    toForm model

                ( newFormState, maybeOutput ) =
                    Form.update form formMsg model.formState

                effect =
                    case ( formMsg, maybeOutput ) of
                        ( Submit, Just output ) ->
                            Effect.fromCmd <| toQuery output

                        _ ->
                            Effect.none
            in
            { model | formState = newFormState, formOutput = maybeOutput }
                |> Tuple.Extra.pairWith effect

        LogInSuccess user ->
            { model | errorState = Nothing }
                |> Tuple.Extra.pairWith (Effect.fromShared <| Shared.LogIn user)

        LogInError error ->
            { model | errorState = Just (error |> Debug.log "err") }
                |> Tuple.Extra.pairWith Effect.none

        CloseError ->
            { model | errorState = Nothing }
                |> Tuple.Extra.pairWith Effect.none


view : Model -> View Msg
view model =
    let
        form =
            toForm model

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
                            (APIError.toText error |> Element.text)
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
    { title = "log-in"
    , body =
        Element.column fillAttr
            [ errorPopup
            , form.view model.formState
                |> Element.column
                    [ Element.centerX
                    , Element.centerY
                    , Element.spacing 10
                    , Element.width (Element.px 300)
                    ]
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
    Form.State FormError FormValue Void


initFormState =
    { value = { email = "", password = "" }
    , error = []
    , animation = Void
    }


type FormError
    = EmailError String


type alias FormValue =
    { email : String
    , password : String
    }


type FormMsg
    = TypeEmail String
    | TypePassword String
    | Submit


type alias FormOutput =
    { email : Email
    , password : String
    }


toForm : Model -> Form FormError FormValue Void FormMsg FormOutput
toForm model =
    let
        emailForm : Form FormError FormValue Void FormMsg Email
        emailForm =
            Form.Email.form
                |> Form.mapValue ( \newVal value -> { value | email = newVal }, .email )
                |> Form.mapError
                    ( EmailError
                    , \err ->
                        case err of
                            EmailError str ->
                                Just str
                    )
                |> Form.mapMsg
                    ( \msg ->
                        case msg of
                            Form.Email.Type str ->
                                TypeEmail str

                            Form.Email.Enter ->
                                Submit
                    , \msg ->
                        case msg of
                            TypeEmail str ->
                                Just (Form.Email.Type str)

                            Submit ->
                                Just Form.Email.Enter

                            _ ->
                                Nothing
                    )

        passwordForm : Form FormError FormValue Void FormMsg String
        passwordForm =
            Form.CurrentPassword.form
                |> Form.mapValue ( \newVal value -> { value | password = newVal }, .password )
                |> Form.mapError ( always (EmailError ""), always Nothing )
                |> Form.mapMsg
                    ( \msg ->
                        case msg of
                            Form.CurrentPassword.Type str ->
                                TypePassword str

                            Form.CurrentPassword.Enter ->
                                Submit
                    , \msg ->
                        case msg of
                            TypePassword str ->
                                Just (Form.CurrentPassword.Type str)

                            Submit ->
                                Just Form.CurrentPassword.Enter

                            _ ->
                                Nothing
                    )

        form_ : Form FormError FormValue Void FormMsg FormOutput
        form_ =
            Form.succeed FormOutput
                |> Form.append emailForm
                |> Form.append passwordForm

        submitButton =
            Element.Input.button
                [ Element.alignRight
                , Element.padding 7
                , Element.Background.color
                    (if model.formOutput == Nothing then
                        Element.rgb 0.6 0.6 0.6

                     else
                        Element.rgb 0.85 0 0
                    )
                , Element.Font.color (Element.rgb 1 1 1)
                , Element.Font.bold
                , Element.Border.rounded 3
                ]
                { onPress = Just Submit
                , label = Element.text "Log In"
                }
    in
    { form_ | view = \state -> form_.view state ++ [ submitButton ] }



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
                    APIError.Http error |> LogInError

                Err (Graphql.Http.GraphqlError _ errors) ->
                    APIError.Graphql errors |> LogInError
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
                        APIError.Server { code = code, message = message }
                            |> LogInError
                    )
                    API.Object.Error.code
                    API.Object.Error.message
            }
        )
        |> Graphql.Http.queryRequest Shared.apiUrl
        |> Graphql.Http.send handleHttpErrors
