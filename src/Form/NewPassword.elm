module Form.NewPassword exposing (..)

import Element
import Element.Border
import Element.Events.Extra
import Element.Extra
import Element.Input
import Form exposing (Form)
import ZxcvbnPlus


type Msg
    = OnEnter
    | NewVal String


type Entry
    = First
    | Confirmation


{-| give a list of strings that correspond to the website to avoid having the website's name in the password
-}
form : List String -> Form ( Entry, String ) ( ZxcvbnPlus.ZxcvbnPlusResult, String ) () ( Entry, Msg ) String
form lexicon =
    let
        first : Form String ZxcvbnPlus.ZxcvbnPlusResult () Msg String
        first =
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
                        NewVal str ->
                            ZxcvbnPlus.zxcvbnPlus lexicon str

                        OnEnter ->
                            value
            , view =
                \{ value, error } ->
                    Element.Input.newPassword
                        [ Element.Events.Extra.onEnter OnEnter
                        , if List.isEmpty error then
                            Element.Extra.noneAttribute

                          else
                            Element.Border.color (Element.rgb 1 0 0)
                        ]
                        { onChange = NewVal
                        , text = value.password
                        , placeholder = Element.Input.placeholder [] (Element.text "yourPassword") |> Just
                        , label = Element.Input.labelAbove [] (Element.text "Password")
                        , show = False
                        }
            }
                |> Form.complex

        confirm : Form error String () Msg String
        confirm =
            { parse = Ok
            , transform =
                \msg { value } ->
                    case msg of
                        NewVal str ->
                            str

                        OnEnter ->
                            value
            , view =
                \{ value, error } ->
                    Element.Input.newPassword
                        [ Element.Events.Extra.onEnter OnEnter
                        , if List.isEmpty error then
                            Element.Extra.noneAttribute

                          else
                            Element.Border.color (Element.rgb 1 0 0)
                        ]
                        { onChange = NewVal
                        , text = value
                        , placeholder = Element.Input.placeholder [] (Element.text "yourPassword") |> Just
                        , label = Element.Input.labelAbove [] (Element.text "Confirm password")
                        , show = False
                        }
            }
                |> Form.complex

        firstMap =
            let
                pairWithEntry =
                    ( Tuple.pair First
                    , \( entry, msg ) ->
                        if entry == First then
                            Just msg

                        else
                            Nothing
                    )
            in
            Form.mapMsg pairWithEntry
                >> Form.mapValue
                    ( \firstVal ( _, confirmVal ) -> ( firstVal, confirmVal ), Tuple.first )
                >> Form.mapError pairWithEntry

        confirmMap =
            let
                pairWithEntry =
                    ( Tuple.pair Confirmation
                    , \( entry, msg ) ->
                        if entry == Confirmation then
                            Just msg

                        else
                            Nothing
                    )
            in
            Form.mapMsg pairWithEntry
                >> Form.mapValue
                    ( \confirmVal ( firstVal, _ ) -> ( firstVal, confirmVal ), Tuple.second )
                >> Form.mapError pairWithEntry
    in
    Form.succeed Tuple.pair
        |> Form.append (firstMap first)
        |> Form.append (confirmMap confirm)
        |> Form.constrain
            (\( a, b ) ->
                if a.password == b then
                    Nothing

                else
                    Just ( Confirmation, "Do not match the password given above" )
            )
        |> Form.map Tuple.first


initValue : ( ZxcvbnPlus.ZxcvbnPlusResult, String )
initValue =
    ( ZxcvbnPlus.zxcvbnPlus [] "", "" )



-- import Serialize exposing (Codec)
-- errorCodec : Codec e ( Entry, String )
-- errorCodec =
--     Serialize.tuple entryCodec Serialize.string
-- entryCodec : Codec e Entry
-- entryCodec =
--     Serialize.customType
--         (\firstEncoder confirmEncoder value ->
--             case value of
--                 First ->
--                     firstEncoder
--                 Confirmation ->
--                     confirmEncoder
--         )
--         |> Serialize.variant0 First
--         |> Serialize.variant0 Confirmation
--         |> Serialize.finishCustomType
