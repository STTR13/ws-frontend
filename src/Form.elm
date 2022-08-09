module Form exposing (..)

{-| heavily inspired by hecrj/composable-form
-}

import Element exposing (Element)


type alias Form state msg output =
    { update : msg -> state -> ( state, Maybe output )
    , view : state -> List (Element msg)
    }


type alias Field error value output =
    { parse : value -> Result error output
    , transform : value -> value
    , view : FieldState error value -> Element value
    }


type alias FieldState error value =
    { value : value
    , error : Maybe error
    }


succeed : output -> Form state msg output
succeed output =
    { update = \_ state -> ( state, Just output )
    , view = always []
    }


append : Form state msg a -> Form state msg (a -> b) -> Form state msg b
append a b =
    { update =
        \msg formState ->
            let
                ( newFormState, maybeOutputB ) =
                    b.update msg formState

                ( newNewFormState, maybeOutputA ) =
                    a.update msg newFormState
            in
            ( newNewFormState
            , Maybe.map2
                (\outputA outputB ->
                    outputB outputA
                )
                (maybeOutputA |> Debug.log "appendA")
                (maybeOutputB |> Debug.log "appendB")
            )
    , view = \formState -> b.view formState ++ a.view formState
    }


appendSubmitButton :
    { isSubmit : msg -> Bool
    , view : Element msg
    }
    -> Form state msg output
    -> Form state msg output
appendSubmitButton { isSubmit, view } form =
    { update =
        \msg formState ->
            let
                ( newFormState, maybeOutput ) =
                    form.update msg formState
            in
            ( newFormState
            , if isSubmit msg then
                maybeOutput
                    |> Debug.log "isSubmit"

              else
                Nothing
            )
    , view = \formState -> form.view formState ++ [ view ]
    }


type alias Interface state msg error value =
    { fieldState :
        { get : state -> FieldState error value
        , insert : FieldState error value -> state -> state
        }
    , msg :
        { fromValue : value -> msg
        , toValue : msg -> Maybe value
        }
    }


toForm :
    Interface state msg error value
    -> Field error value output
    -> Form state msg output
toForm interface field =
    let
        applyNewValue : value -> ( FieldState error value, Maybe output )
        applyNewValue value =
            let
                newVal =
                    field.transform value
            in
            case field.parse newVal |> Debug.log "parse" of
                Ok output ->
                    ( { value = newVal, error = Nothing }
                    , Just output
                    )

                Err error ->
                    ( { value = newVal, error = Just error }
                    , Nothing
                    )
    in
    { update =
        \msg formState ->
            let
                ( newFieldState, output ) =
                    interface.msg.toValue msg
                        |> Maybe.withDefault
                            (interface.fieldState.get formState |> .value)
                        |> applyNewValue
            in
            ( interface.fieldState.insert newFieldState formState
            , output
            )
    , view =
        \formState ->
            field.view (interface.fieldState.get formState)
                |> Element.map interface.msg.fromValue
                |> List.singleton
    }


map : (a -> b) -> Form state msg a -> Form state msg b
map func form =
    { update =
        \msg state ->
            form.update msg state
                |> Tuple.mapSecond (Maybe.map func)
    , view = form.view
    }
