module Pages.CreateUser exposing (Model, Msg, page)

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
import Form.Email
import Form.NewPassword
import Graphql.Http
import Graphql.SelectionSet
import Material.Icons as Icon
import Material.Icons.Types as Icon_Color
import Page
import Request exposing (Request)
import Shared
import Svg
import Svg.Attributes
import TabIndex
import Tuple.Extra
import View exposing (View)
import ZxcvbnPlus


type alias Model =
    { formState : FormState
    , formOutput : Maybe FormOutput
    , errorState : Maybe APIError.Error
    }


type Msg
    = FormUpdate FormMsg
    | CreateUserSuccess
    | CreateUserError APIError.Error
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

                        ( FocusNext, _ ) ->
                            Effect.fromCmd <| TabIndex.focusNext ()

                        _ ->
                            Effect.none
            in
            { model | formState = newFormState, formOutput = maybeOutput }
                |> Tuple.Extra.pairWith effect

        CreateUserSuccess ->
            { model | errorState = Nothing }
                |> Tuple.Extra.pairWith Effect.none

        CreateUserError error ->
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
                                Icon.close 10
                                    (Icon_Color.Color <| Color.rgb 1 1 1)
                                    |> List.singleton
                                    |> Svg.svg [ Svg.Attributes.width "10", Svg.Attributes.height "10", Svg.Attributes.viewBox "0 0 10 10" ]
                                    |> Element.html
                            }
                        ]
    in
    { title = "create user"
    , body =
        Element.column fillAttr
            [ errorPopup
            , Element.column
                [ Element.centerX
                , Element.centerY
                , Element.spacing 10
                , Element.width (Element.px 300)
                ]
                (form.view model.formState)
                |> Element.map FormUpdate
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
    Form.State FormError FormValue ()


initFormState : FormState
initFormState =
    { value = { email = "", password = Form.NewPassword.initValue, isAdmin = False }
    , error = []
    , animation = ()
    }


type FormError
    = EmailError String
    | PasswordError ( Form.NewPassword.Entry, String )


type alias FormValue =
    { email : String
    , password : ( ZxcvbnPlus.ZxcvbnPlusResult, String )
    , isAdmin : Bool
    }


type FormMsg
    = TypeEmail String
    | TypePassword ( Form.NewPassword.Entry, String )
    | ToggleIsAdmin Bool
    | FocusNext
    | Submit


type alias FormOutput =
    { email : Email
    , password : String
    , isAdmin : Bool
    }


toForm : Model -> Form FormError FormValue () FormMsg FormOutput
toForm model =
    let
        emailForm : Form FormError FormValue () FormMsg Email
        emailForm =
            Form.Email.form
                |> Form.mapError
                    ( EmailError
                    , \err ->
                        case err of
                            EmailError str ->
                                Just str

                            _ ->
                                Nothing
                    )
                |> Form.mapMsg
                    ( \msg ->
                        case msg of
                            Form.Email.NewVal str ->
                                TypeEmail str

                            Form.Email.OnEnter ->
                                FocusNext
                    , \msg ->
                        case msg of
                            TypeEmail str ->
                                Just (Form.Email.NewVal str)

                            _ ->
                                Nothing
                    )
                |> Form.mapValue ( \newVal value -> { value | email = newVal }, .email )

        passwordForm : Form FormError FormValue () FormMsg String
        passwordForm =
            Form.NewPassword.form [ "workstation", "work", "station", "ws", "42ai", "42", "ai" ]
                |> Form.mapError
                    ( PasswordError
                    , \msg ->
                        case msg of
                            PasswordError err ->
                                Just err

                            _ ->
                                Nothing
                    )
                |> Form.mapValue ( \newVal value -> { value | password = newVal }, .password )
                |> Form.mapMsg
                    ( \( entry, msg ) ->
                        case ( entry, msg ) of
                            ( _, Form.NewPassword.NewVal str ) ->
                                TypePassword ( entry, str )

                            ( Form.NewPassword.First, Form.NewPassword.OnEnter ) ->
                                FocusNext

                            ( Form.NewPassword.Confirmation, Form.NewPassword.OnEnter ) ->
                                Submit
                    , \msg ->
                        case msg of
                            TypePassword ( entry, str ) ->
                                Just ( entry, Form.NewPassword.NewVal str )

                            _ ->
                                Nothing
                    )

        isAdmin : Form FormError FormValue () FormMsg Bool
        isAdmin =
            Form.simple
                { parse = Ok
                , view =
                    \{ value } ->
                        Element.Input.checkbox
                            [ Element.padding 7
                            , Element.Font.color (Element.rgb 1 1 1)
                            , Element.Font.bold
                            , Element.Border.rounded 3
                            ]
                            { onChange = identity
                            , icon =
                                \bool ->
                                    (if bool then
                                        Icon.check_box 18 (Color.rgb 0.85 0 0 |> Icon_Color.Color)

                                     else
                                        Icon.check_box_outline_blank 18 (Color.rgb 0.7 0.7 0.7 |> Icon_Color.Color)
                                    )
                                        |> List.singleton
                                        |> Svg.svg [ Svg.Attributes.width "18", Svg.Attributes.height "18", Svg.Attributes.viewBox "0 0 18 18" ]
                                        |> Element.html
                            , checked = value
                            , label =
                                if value then
                                    Element.Input.labelRight
                                        [ Element.Font.color (Element.rgb 0.85 0 0) ]
                                        (Element.text "Gets admin rights")

                                else
                                    Element.Input.labelRight
                                        [ Element.Font.color (Element.rgb 0.7 0.7 0.7) ]
                                        (Element.text "Do not get admin rights")
                            }
                }
                |> Form.mapMsg
                    ( ToggleIsAdmin
                    , \msg ->
                        case msg of
                            ToggleIsAdmin bool ->
                                Just bool

                            _ ->
                                Nothing
                    )
                |> Form.mapValue ( \newVal value -> { value | isAdmin = newVal }, .isAdmin )

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
                { onPress =
                    if model.formOutput == Nothing then
                        Nothing

                    else
                        Just Submit
                , label = Element.text "Create User"
                }
    in
    Form.succeed FormOutput
        |> Form.append emailForm
        |> Form.append passwordForm
        |> Form.append isAdmin
        |> (\form -> { form | view = \state -> form.view state ++ [ submitButton ] })



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
                    APIError.Http error |> CreateUserError

                Err (Graphql.Http.GraphqlError _ errors) ->
                    APIError.Graphql errors |> CreateUserError
    in
    API.Query.login
        { id = Email.toString email, pwd = password }
        (API.Union.LoginRes.fragments
            { onToken =
                Graphql.SelectionSet.map
                    (always CreateUserSuccess)
                    API.Object.Token.token
            , onError =
                Graphql.SelectionSet.map2
                    (\code message ->
                        APIError.Server { code = code, message = message }
                            |> CreateUserError
                    )
                    API.Object.Error.code
                    API.Object.Error.message
            }
        )
        |> Graphql.Http.queryRequest Shared.apiUrl
        |> Graphql.Http.send handleHttpErrors
