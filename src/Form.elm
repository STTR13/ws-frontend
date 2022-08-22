module Form exposing (..)

import Dict exposing (Dict)
import Element.Pipe
import List.Extra


type alias Form error value animation msg output =
    { parse : value -> Result error output
    , transform : msg -> SimplifiedState error value -> value
    , animate : State error value animation -> animation
    , view : State error value animation -> Element.Pipe.Builder msg
    }


type alias State error value animation =
    { value : value
    , error : Maybe error
    , animation : animation
    }


type alias SimplifiedState error value =
    { value : value
    , error : Maybe error
    }



-- ███████╗██╗███████╗██╗     ██████╗     ██╗███╗   ██╗██╗████████╗
-- ██╔════╝██║██╔════╝██║     ██╔══██╗    ██║████╗  ██║██║╚══██╔══╝
-- █████╗  ██║█████╗  ██║     ██║  ██║    ██║██╔██╗ ██║██║   ██║
-- ██╔══╝  ██║██╔══╝  ██║     ██║  ██║    ██║██║╚██╗██║██║   ██║
-- ██║     ██║███████╗███████╗██████╔╝    ██║██║ ╚████║██║   ██║
-- ╚═╝     ╚═╝╚══════╝╚══════╝╚═════╝     ╚═╝╚═╝  ╚═══╝╚═╝   ╚═╝


type Void
    = Void


succeed : Element.Pipe.Orientation -> a -> Form error value animation msg a
succeed orientation a =
    { parse = always (Ok a)
    , transform = always .value
    , animate = .animation
    , view = always (Element.Pipe.empty orientation)
    }


simple :
    { parse : value -> Result error output
    , view : SimplifiedState error value -> Element.Pipe.Builder value
    }
    -> Form error value Void value output
simple form =
    { parse = form.parse
    , transform = \value _ -> value
    , animate = .animation
    , view =
        \formState ->
            form.view { value = formState.value, error = formState.error }
    }


complex :
    { parse : value -> Result error output
    , transform : msg -> SimplifiedState error value -> value
    , view : SimplifiedState error value -> Element.Pipe.Builder msg
    }
    -> Form error value Void msg output
complex form =
    { parse = form.parse
    , transform = form.transform
    , animate = .animation
    , view =
        \formState ->
            form.view { value = formState.value, error = formState.error }
    }


complete :
    { parse : value -> Result error output
    , transform : msg -> SimplifiedState error value -> value
    , animate : State error value animation -> animation
    , view : State error value animation -> Element.Pipe.Builder msg
    }
    -> Form error value animation msg output
complete =
    identity



-- ██╗   ██╗███████╗███████╗
-- ██║   ██║██╔════╝██╔════╝
-- ██║   ██║███████╗█████╗
-- ██║   ██║╚════██║██╔══╝
-- ╚██████╔╝███████║███████╗
--  ╚═════╝ ╚══════╝╚══════╝


view : Form error value animation msg output -> State error value animation -> Element.Pipe.Builder msg
view form state =
    form.view state


update : Form error value animation msg output -> msg -> State error value animation -> ( State error value animation, Maybe output )
update form msg state =
    let
        newVal =
            form.transform msg
                { value = state.value, error = state.error }
    in
    case form.parse newVal of
        Ok output ->
            ( { state | value = newVal, error = Nothing }, Just output )

        Err error ->
            ( { state | value = newVal, error = Just error }, Nothing )


animate : Form error value animation msg output -> State error value animation -> State error value animation
animate form state =
    { state | animation = form.animate state }



-- ███╗   ███╗ █████╗ ██████╗
-- ████╗ ████║██╔══██╗██╔══██╗
-- ██╔████╔██║███████║██████╔╝
-- ██║╚██╔╝██║██╔══██║██╔═══╝
-- ██║ ╚═╝ ██║██║  ██║██║
-- ╚═╝     ╚═╝╚═╝  ╚═╝╚═╝


map : (a -> b) -> Form error value animation msg a -> Form error value animation msg b
map func form =
    { parse = form.parse >> Result.map func
    , transform = form.transform
    , animate = form.animate
    , view = form.view
    }


mapError : ( a -> b, b -> Maybe a ) -> Form a value animation msg output -> Form b value animation msg output
mapError ( func, invFunc ) form =
    { parse = form.parse >> Result.mapError func
    , transform =
        \msg simplifiedState ->
            form.transform msg
                { value = simplifiedState.value
                , error = Maybe.andThen invFunc simplifiedState.error
                }
    , animate =
        \state ->
            form.animate
                { value = state.value
                , error = Maybe.andThen invFunc state.error
                , animation = state.animation
                }
    , view =
        \state ->
            form.view
                { value = state.value
                , error = Maybe.andThen invFunc state.error
                , animation = state.animation
                }
    }


mapValue : ( a -> b -> b, b -> a ) -> Form error a animation msg output -> Form error b animation msg output
mapValue ( func, invFunc ) form =
    { parse = invFunc >> form.parse
    , transform =
        \msg simplifiedState ->
            let
                newVal =
                    form.transform msg
                        { value = invFunc simplifiedState.value
                        , error = simplifiedState.error
                        }
            in
            func newVal simplifiedState.value
    , animate =
        \state ->
            form.animate
                { value = invFunc state.value
                , error = state.error
                , animation = state.animation
                }
    , view =
        \state ->
            form.view
                { value = invFunc state.value
                , error = state.error
                , animation = state.animation
                }
    }


mapAnimation : ( a -> b, b -> a ) -> Form error value a msg output -> Form error value b msg output
mapAnimation ( func, invFunc ) form =
    { parse = form.parse
    , transform = form.transform
    , animate =
        \state ->
            form.animate
                { value = state.value
                , error = state.error
                , animation = invFunc state.animation
                }
                |> func
    , view =
        \state ->
            form.view
                { value = state.value
                , error = state.error
                , animation = invFunc state.animation
                }
    }


mapMsg : ( a -> b, b -> Maybe a ) -> Form error value animation a output -> Form error value animation b output
mapMsg ( func, invFunc ) form =
    { parse = form.parse
    , transform =
        \msg simplifiedState ->
            invFunc msg
                |> Maybe.map (\formMsg -> form.transform formMsg simplifiedState)
                |> Maybe.withDefault simplifiedState.value
    , animate = form.animate
    , view = form.view >> Element.Pipe.map func
    }



--  ██████╗ ██████╗ ███╗   ███╗██████╗ ██╗███╗   ██╗███████╗
-- ██╔════╝██╔═══██╗████╗ ████║██╔══██╗██║████╗  ██║██╔════╝
-- ██║     ██║   ██║██╔████╔██║██████╔╝██║██╔██╗ ██║█████╗
-- ██║     ██║   ██║██║╚██╔╝██║██╔══██╗██║██║╚██╗██║██╔══╝
-- ╚██████╗╚██████╔╝██║ ╚═╝ ██║██████╔╝██║██║ ╚████║███████╗
--  ╚═════╝ ╚═════╝ ╚═╝     ╚═╝╚═════╝ ╚═╝╚═╝  ╚═══╝╚══════╝


append :
    Form (List error) value animation msg a
    -> Form (List error) value animation msg (a -> b)
    -> Form (List error) value animation msg b
append formA formB =
    { parse =
        \value ->
            case ( formA.parse value, formB.parse value ) of
                ( Ok a, Ok b ) ->
                    b a |> Ok

                ( Err err, Ok _ ) ->
                    Err err

                ( Ok _, Err err ) ->
                    Err err

                ( Err errA, Err errB ) ->
                    Err (errB ++ errA)
    , transform =
        \msg state ->
            formA.transform msg { state | value = formB.transform msg state }
    , animate =
        \state ->
            formA.animate { state | animation = formB.animate state }
    , view =
        \state ->
            formB.view state
                |> Element.Pipe.append (view formA state |> Element.Pipe.toElement)
    }


list :
    Element.Pipe.Orientation
    -> Form error value animation msg output
    -> Form (Dict Int error) (List value) (List animation) ( Int, msg ) (List output)
list orientation form =
    let
        parse_ : List value -> Result (Dict Int error) (List output)
        parse_ valList =
            List.Extra.indexedFoldr
                (\index val output ->
                    case form.parse val of
                        Ok out ->
                            Result.map ((::) out) output

                        Err err ->
                            Result.mapError (Dict.insert index err) output
                                |> Result.andThen (always (Err (Dict.singleton index err)))
                )
                (Ok [])
                valList

        transform_ : ( Int, msg ) -> SimplifiedState (Dict Int error) (List value) -> List value
        transform_ ( msgIndex, msg ) formListState =
            List.Extra.indexedFoldr
                (\index formValue acc ->
                    (if msgIndex == index then
                        form.transform msg
                            { value = formValue
                            , error = Maybe.andThen (Dict.get index) formListState.error
                            }

                     else
                        formValue
                    )
                        :: acc
                )
                []
                formListState.value

        animate_ : State (Dict Int error) (List value) (List animation) -> List animation
        animate_ formListState =
            List.Extra.indexedFoldr
                (\index ( formValue, formAnimation ) acc ->
                    form.animate
                        { value = formValue
                        , error = Maybe.andThen (Dict.get index) formListState.error
                        , animation = formAnimation
                        }
                        :: acc
                )
                []
                (List.Extra.zip formListState.value formListState.animation)

        view_ : State (Dict Int error) (List value) (List animation) -> Element.Pipe.Builder ( Int, msg )
        view_ formListState =
            List.Extra.indexedFoldr
                (\index ( formValue, formAnimation ) ->
                    Element.Pipe.append
                        (form.view
                            { value = formValue
                            , error = Maybe.andThen (Dict.get index) formListState.error
                            , animation = formAnimation
                            }
                            |> Element.Pipe.map (Tuple.pair index)
                            |> Element.Pipe.toElement
                        )
                )
                (Element.Pipe.empty orientation)
                (List.Extra.zip formListState.value formListState.animation)
    in
    { parse = parse_, transform = transform_, animate = animate_, view = view_ }
