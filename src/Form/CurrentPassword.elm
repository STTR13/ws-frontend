module Form.CurrentPassword exposing (Msg(..), form)

import Element
import Element.Events.Extra
import Element.Input
import Form exposing (Form)
import Void exposing (..)


type Msg
    = Enter
    | Type String


form : Form Never String Void Msg String
form =
    Form.complex
        { parse = Ok
        , transform =
            \msg { value } ->
                case msg of
                    Type str ->
                        str

                    _ ->
                        value
        , view =
            \{ value } ->
                Element.Input.currentPassword
                    [ Element.Events.Extra.onEnter Enter ]
                    { onChange = Type
                    , text = value
                    , placeholder = Element.Input.placeholder [] (Element.text "yourPassword") |> Just
                    , label = Element.Input.labelAbove [] (Element.text "Password")
                    , show = False
                    }
        }
