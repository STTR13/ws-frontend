module Form.Email exposing (Msg(..), form)

import Element
import Element.Border
import Element.Events.Extra
import Element.Extra
import Element.Input
import Email exposing (Email)
import Form exposing (Form)


type Msg
    = OnEnter
    | NewVal String


form : Form String String () Msg Email
form =
    Form.complex
        { parse = Email.fromString >> Result.fromMaybe "invalid email"
        , transform =
            \msg { value } ->
                case msg of
                    NewVal str ->
                        str

                    _ ->
                        value
        , view =
            \{ value, error } ->
                Element.Input.email
                    [ if List.isEmpty error then
                        Element.Extra.noneAttribute

                      else
                        Element.Border.color (Element.rgb 1 0 0)
                    , Element.Events.Extra.onEnter OnEnter
                    ]
                    { onChange = NewVal
                    , text = value
                    , placeholder = Element.Input.placeholder [] (Element.text "your@email.com") |> Just
                    , label = Element.Input.labelAbove [] (Element.text "Email")
                    }
        }
