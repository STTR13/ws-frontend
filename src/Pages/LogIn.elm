module Pages.LogIn exposing (Model, Msg, page)

import API.Object.Error
import API.Object.Token
import API.Query
import API.Union.LoginRes
import Effect exposing (Effect)
import Element exposing (Element)
import Email exposing (Email)
import Form
import Form.View
import Graphql.Http
import Graphql.Http.GraphqlError
import Graphql.SelectionSet
import Page
import Request exposing (Request)
import Shared
import Tuple.Extra
import View exposing (View)


type alias Model =
    { formModel : FormModel }


type Msg
    = FormUpdate FormModel
    | FormSubmit FormOutput
    | LogInSuccess Shared.User
    | ServerError { code : Int, message : String }
    | HttpError Graphql.Http.HttpError
    | GraphqlError (List Graphql.Http.GraphqlError.GraphqlError)


init : ( Model, Effect Msg )
init =
    ( { formModel = Form.View.idle { email = "", password = "" } }
    , Effect.none
    )


update : Msg -> Model -> ( Model, Effect Msg )
update msg model =
    case msg of
        FormUpdate newFormModel ->
            { model | formModel = newFormModel }
                |> Tuple.Extra.pairWith Effect.none

        FormSubmit formOutput ->
            { model
                | formModel = setState Form.View.Loading model.formModel
            }
                |> Tuple.Extra.pairWith (Effect.fromCmd <| toQuery formOutput)

        LogInSuccess user ->
            let
                message =
                    "You're logged in!"
            in
            { model
                | formModel = setState (Form.View.Success message) model.formModel
            }
                |> Tuple.Extra.pairWith (Effect.fromShared <| Shared.LogIn user)

        ServerError { message } ->
            { model
                | formModel = setState (Form.View.Error message) model.formModel
            }
                |> Tuple.Extra.pairWith Effect.none

        HttpError httpErr ->
            let
                message =
                    httpErrorToHumanReadableString httpErr
            in
            { model
                | formModel = setState (Form.View.Error message) model.formModel
            }
                |> Tuple.Extra.pairWith Effect.none

        GraphqlError _ ->
            let
                message =
                    "There is an issue in the program (graphql).\nPlease post an issue on github"
            in
            { model
                | formModel = setState (Form.View.Error message) model.formModel
            }
                |> Tuple.Extra.pairWith Effect.none


view : Model -> View Msg
view model =
    { title = "sign in"
    , body = viewForm model.formModel
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


type alias FormValues =
    { email : String
    , password : String
    }


type alias FormOutput =
    { email : Email
    , password : String
    }


type alias FormModel =
    Form.View.Model FormValues


viewForm : FormModel -> Element Msg
viewForm formModel =
    let
        emailField =
            Form.emailField
                { parser = Email.fromString >> Result.fromMaybe "email invalid"
                , value = .email
                , update = \value values -> { values | email = value }
                , error = always Nothing
                , attributes =
                    { label = "E-Mail"
                    , placeholder = "your@email.com"
                    }
                }

        passwordField =
            Form.passwordField
                { parser = Ok
                , value = .password
                , update = \value values -> { values | password = value }
                , error = always Nothing
                , attributes =
                    { label = "Password"
                    , placeholder = "Your password"
                    }
                }
    in
    Form.View.asHtml
        { onChange = FormUpdate
        , action = "Log in"
        , loading = "Logging in..."
        , validation = Form.View.ValidateOnSubmit
        }
        (Form.succeed FormOutput
            |> Form.append emailField
            |> Form.append passwordField
            |> Form.map FormSubmit
        )
        formModel
        |> Element.html


setState : Form.View.State -> FormModel -> FormModel
setState newState formModel =
    { formModel | state = newState }



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
                    HttpError error

                Err (Graphql.Http.GraphqlError _ errors) ->
                    GraphqlError errors
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
                    )
                    API.Object.Error.code
                    API.Object.Error.message
            }
        )
        |> Graphql.Http.queryRequest Shared.apiUrl
        |> Graphql.Http.send handleHttpErrors


httpErrorToHumanReadableString : Graphql.Http.HttpError -> String
httpErrorToHumanReadableString httpErr =
    case httpErr of
        Graphql.Http.Timeout ->
            "The communication with the server timed out.\nPlease make sure your internet connection is working properly"

        Graphql.Http.NetworkError ->
            "There was an issue when we tried reaching to the server.\nPlease make sure your internet connection is working properly"

        _ ->
            "There is an issue in the program (http).\nPlease post an issue on github"
