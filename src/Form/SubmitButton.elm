module Form.SubmitButton exposing (..)

{-| This is test code that ended up being more cumbersome to use than anything
-}

import APIError exposing (Error)
import Element
import Element.Background
import Element.Border
import Element.Font
import Element.Input
import Form exposing (Form)
import NeList exposing (NeList)


type Msg msg
    = Msg msg
    | Submit


type Error error
    = Error error
    | NoSubmit


type alias Value error value output =
    { value : value, result : Result (NeList error) output, submit : Bool }


initValue form value =
    { value = value, result = form.parse value, submit = False }


addSubmitButton :
    String
    -> Form error value animation msg output
    -> Form (Error error) (Value error value output) animation (Msg msg) output
addSubmitButton label form =
    let
        submitButtonForm : Form (Error error) (Value error value output) a (Msg msg) ()
        submitButtonForm =
            Form.complete
                { parse =
                    \{ submit } ->
                        if submit then
                            Ok ()

                        else
                            Err NoSubmit
                , transform =
                    \msg { value } ->
                        case msg of
                            Submit ->
                                { value | submit = True }

                            _ ->
                                { value | submit = False }
                , animate = .animation
                , view =
                    \{ value } ->
                        Element.Input.button
                            [ Element.alignRight
                            , Element.padding 7
                            , Element.Background.color
                                (case value.result of
                                    Ok _ ->
                                        Element.rgb 0.85 0 0

                                    Err _ ->
                                        Element.rgb 0.6 0.6 0.6
                                )
                            , Element.Font.color (Element.rgb 1 1 1)
                            , Element.Font.bold
                            , Element.Border.rounded 3
                            ]
                            { onPress =
                                case value.result of
                                    Ok _ ->
                                        Just Submit

                                    Err _ ->
                                        Nothing
                            , label = Element.text label
                            }
                }
    in
    form
        |> Form.mapError
            ( Error
            , \err ->
                case err of
                    Error e ->
                        Just e

                    NoSubmit ->
                        Nothing
            )
        |> Form.mapValue
            ( \value parentVal ->
                { parentVal | value = value, result = form.parse value }
            , .value
            )
        |> Form.mapMsg
            ( Msg
            , \msg ->
                case msg of
                    Msg msg_ ->
                        Just msg_

                    Submit ->
                        Nothing
            )
        |> Form.add submitButtonForm
