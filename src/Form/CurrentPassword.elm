module Form.CurrentPassword exposing (Msg(..), form)

import Element
import Element.Events.Extra
import Element.Input
import Form exposing (Form)


type Msg
    = OnEnter
    | NewVal String


form : Form Never String () Msg String
form =
    Form.complex
        { parse = Ok
        , transform =
            \msg { value } ->
                case msg of
                    NewVal str ->
                        str

                    _ ->
                        value
        , view =
            \{ value } ->
                Element.Input.currentPassword
                    [ Element.Events.Extra.onEnter OnEnter ]
                    { onChange = NewVal
                    , text = value
                    , placeholder = Element.Input.placeholder [] (Element.text "yourPassword") |> Just
                    , label = Element.Input.labelAbove [] (Element.text "Password")
                    , show = False
                    }
        }
