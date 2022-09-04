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
import Graphql.Http
import Graphql.SelectionSet
import Html.Attributes
import Html.Events
import Json.Decode as Decode
import Material.Icons as Icon
import Material.Icons.Types as Icon_Color
import Page
import Request exposing (Request)
import Shared
import Svg
import Svg.Attributes
import Tuple.Extra
import View exposing (View)
import Void exposing (..)
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
    Form.State FormError FormValue Void


initFormState : FormState
initFormState =
    { value = { email = "", password = ZxcvbnPlus.zxcvbnPlus [] "", isAdmin = False, submit = False }
    , error = []
    , animation = Void
    }


type FormError
    = EmailError String
    | PasswordError String
    | NoSubmit


type alias FormValue =
    { email : String
    , password : ZxcvbnPlus.ZxcvbnPlusResult
    , isAdmin : Bool
    , submit : Bool
    }


type FormMsg
    = TypeEmail String
    | TypePassword String
    | ToggleIsAdmin Bool
    | Submit


type alias FormOutput =
    { email : Email
    , password : String
    , isAdmin : Bool
    }


form : Form FormError FormValue Void FormMsg FormOutput
form =
    let
        emailForm : Form FormError FormValue Void FormMsg Email
        emailForm =
            Form.simple
                { parse = Email.fromString >> Result.fromMaybe "invalid email"
                , view =
                    \{ value, error } ->
                        Element.Input.email
                            [ if List.isEmpty error then
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
                    ( TypeEmail
                    , \msg ->
                        case msg of
                            TypeEmail str ->
                                Just str

                            _ ->
                                Nothing
                    )
                |> Form.mapValue ( \newVal value -> { value | email = newVal }, .email )

        passwordForm : Form FormError FormValue Void FormMsg String
        passwordForm =
            Form.complex
                { parse =
                    \{ password, score } ->
                        if
                            List.member score
                                [ ZxcvbnPlus.TooGuessable
                                , ZxcvbnPlus.VeryGuessable
                                , ZxcvbnPlus.SomewhatGuessable
                                ]
                        then
                            Err "Password too guessable"

                        else
                            Ok password
                , transform =
                    \msg { value } ->
                        case msg of
                            TypePassword str ->
                                ZxcvbnPlus.zxcvbnPlus
                                    [ "workstation", "work", "station", "ws", "42ai", "42", "ai" ]
                                    str

                            _ ->
                                value
                , view =
                    \{ value, error } ->
                        Element.Input.currentPassword
                            [ onEnter Submit
                            , if List.isEmpty error then
                                noneAttribute

                              else
                                Element.Border.color (Element.rgb 1 0 0)
                            ]
                            { onChange = TypePassword
                            , text = value.password
                            , placeholder = Element.Input.placeholder [] (Element.text "yourPassword") |> Just
                            , label = Element.Input.labelAbove [] (Element.text "Password")
                            , show = False
                            }
                }
                |> Form.mapError
                    ( PasswordError
                    , \msg ->
                        case msg of
                            PasswordError str ->
                                Just str

                            _ ->
                                Nothing
                    )
                |> Form.mapValue ( \newVal value -> { value | password = newVal }, .password )

        isAdmin : Form FormError FormValue Void FormMsg Bool
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

        submitButton : Form FormError FormValue Void FormMsg Void
        submitButton =
            Form.complex
                { parse =
                    \value ->
                        if value then
                            Ok Void

                        else
                            Err NoSubmit
                , transform =
                    \msg _ ->
                        case msg of
                            Submit ->
                                True

                            _ ->
                                False
                , view =
                    \{ value } ->
                        Element.Input.button
                            [ Element.alignRight
                            , Element.padding 7
                            , Element.Background.color
                                (if value then
                                    Element.rgb 0.85 0 0

                                 else
                                    Element.rgb 0.6 0.6 0.6
                                )
                            , Element.Font.color (Element.rgb 1 1 1)
                            , Element.Font.bold
                            , Element.Border.rounded 3
                            ]
                            { onPress = Just Submit
                            , label = Element.text "Create"
                            }
                }
                |> Form.mapValue ( \newVal value -> { value | submit = newVal }, .submit )
    in
    Form.succeed FormOutput
        |> Form.append emailForm
        |> Form.append passwordForm
        |> Form.append isAdmin
        |> Form.add submitButton



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



-- ██╗   ██╗████████╗██╗██╗
-- ██║   ██║╚══██╔══╝██║██║
-- ██║   ██║   ██║   ██║██║
-- ██║   ██║   ██║   ██║██║
-- ╚██████╔╝   ██║   ██║███████╗
--  ╚═════╝    ╚═╝   ╚═╝╚══════╝


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
