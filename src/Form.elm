module Form exposing (..)

import Element exposing (Element)
import List.Extra
import NeList exposing (NeList)
import Void exposing (..)


type alias Form error value animation msg output =
    { parse : value -> Result (NeList error) output
    , transform : msg -> SimplifiedState error value -> value
    , animate : State error value animation -> animation
    , view : State error value animation -> List (Element msg)
    }


type alias State error value animation =
    { value : value
    , error : List error
    , animation : animation
    }


type alias SimplifiedState error value =
    { value : value
    , error : List error
    }



-- ███████╗██╗███████╗██╗     ██████╗     ██╗███╗   ██╗██╗████████╗
-- ██╔════╝██║██╔════╝██║     ██╔══██╗    ██║████╗  ██║██║╚══██╔══╝
-- █████╗  ██║█████╗  ██║     ██║  ██║    ██║██╔██╗ ██║██║   ██║
-- ██╔══╝  ██║██╔══╝  ██║     ██║  ██║    ██║██║╚██╗██║██║   ██║
-- ██║     ██║███████╗███████╗██████╔╝    ██║██║ ╚████║██║   ██║
-- ╚═╝     ╚═╝╚══════╝╚══════╝╚═════╝     ╚═╝╚═╝  ╚═══╝╚═╝   ╚═╝


succeed : a -> Form error value animation msg a
succeed a =
    { parse = always (Ok a)
    , transform = always .value
    , animate = .animation
    , view = always []
    }


simple :
    { parse : value -> Result error output
    , view : SimplifiedState error value -> Element value
    }
    -> Form error value Void value output
simple form =
    { parse = form.parse >> Result.mapError NeList.singleton
    , transform = \value _ -> value
    , animate = .animation
    , view =
        \formState ->
            form.view { value = formState.value, error = formState.error }
                |> List.singleton
    }


complex :
    { parse : value -> Result error output
    , transform : msg -> SimplifiedState error value -> value
    , view : SimplifiedState error value -> Element msg
    }
    -> Form error value Void msg output
complex form =
    { parse = form.parse >> Result.mapError NeList.singleton
    , transform = form.transform
    , animate = .animation
    , view =
        \formState ->
            form.view { value = formState.value, error = formState.error }
                |> List.singleton
    }


complete :
    { parse : value -> Result error output
    , transform : msg -> SimplifiedState error value -> value
    , animate : State error value animation -> animation
    , view : State error value animation -> Element msg
    }
    -> Form error value animation msg output
complete form =
    { parse = form.parse >> Result.mapError NeList.singleton
    , transform = form.transform
    , animate = form.animate
    , view =
        \formState ->
            form.view formState
                |> List.singleton
    }



-- ██╗   ██╗███████╗███████╗
-- ██║   ██║██╔════╝██╔════╝
-- ██║   ██║███████╗█████╗
-- ██║   ██║╚════██║██╔══╝
-- ╚██████╔╝███████║███████╗
--  ╚═════╝ ╚══════╝╚══════╝
-- view :
--     Form error value animation msg output
--     -> State error value animation
--     -> List (Element msg)
-- view form state =
--     form.view state


update : Form error value animation msg output -> msg -> State error value animation -> ( State error value animation, Maybe output )
update form msg state =
    let
        newVal =
            form.transform msg
                { value = state.value, error = state.error }
    in
    case form.parse newVal of
        Ok output ->
            ( { state | value = newVal, error = [] }, Just output )

        Err error ->
            ( { state | value = newVal, error = NeList.toList error }, Nothing )


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
    { parse = form.parse >> Result.mapError (NeList.map func)
    , transform =
        \msg simplifiedState ->
            form.transform msg
                { value = simplifiedState.value
                , error = List.filterMap invFunc simplifiedState.error
                }
    , animate =
        \state ->
            form.animate
                { value = state.value
                , error = List.filterMap invFunc state.error
                , animation = state.animation
                }
    , view =
        \state ->
            form.view
                { value = state.value
                , error = List.filterMap invFunc state.error
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
    , view = form.view >> List.map (Element.map func)
    }



--  ██████╗ ██████╗ ███╗   ███╗██████╗ ██╗███╗   ██╗███████╗
-- ██╔════╝██╔═══██╗████╗ ████║██╔══██╗██║████╗  ██║██╔════╝
-- ██║     ██║   ██║██╔████╔██║██████╔╝██║██╔██╗ ██║█████╗
-- ██║     ██║   ██║██║╚██╔╝██║██╔══██╗██║██║╚██╗██║██╔══╝
-- ╚██████╗╚██████╔╝██║ ╚═╝ ██║██████╔╝██║██║ ╚████║███████╗
--  ╚═════╝ ╚═════╝ ╚═╝     ╚═╝╚═════╝ ╚═╝╚═╝  ╚═══╝╚══════╝


append :
    Form error value animation msg a
    -> Form error value animation msg (a -> b)
    -> Form error value animation msg b
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
                    Err (NeList.append errB errA)
    , transform =
        \msg state ->
            formA.transform msg { state | value = formB.transform msg state }
    , animate =
        \state ->
            formA.animate { state | animation = formB.animate state }
    , view =
        \state ->
            formB.view state ++ formA.view state
    }


add :
    Form error value animation msg Void
    -> Form error value animation msg a
    -> Form error value animation msg a
add formA formB =
    { parse =
        \value ->
            case ( formA.parse value, formB.parse value ) of
                ( Ok Void, Ok b ) ->
                    Ok b

                ( Err err, Ok _ ) ->
                    Err err

                ( Ok Void, Err err ) ->
                    Err err

                ( Err errA, Err errB ) ->
                    Err (NeList.append errB errA)
    , transform =
        \msg state ->
            formA.transform msg { state | value = formB.transform msg state }
    , animate =
        \state ->
            formA.animate { state | animation = formB.animate state }
    , view =
        \state ->
            formB.view state ++ formA.view state
    }


list :
    Form error value animation msg output
    -> Form ( Int, error ) (List value) (List animation) ( Int, msg ) (List output)
list form =
    let
        parse_ : List value -> Result (NeList ( Int, error )) (List output)
        parse_ valList =
            List.Extra.indexedFoldr
                (\index val output ->
                    case form.parse val of
                        Ok out ->
                            Result.map ((::) out) output

                        Err err ->
                            let
                                indexedErr =
                                    NeList.map (Tuple.pair index) err
                            in
                            Result.mapError (NeList.appendWith indexedErr) output
                                |> Result.andThen (always <| Err indexedErr)
                )
                (Ok [])
                valList

        transform_ : ( Int, msg ) -> SimplifiedState ( Int, error ) (List value) -> List value
        transform_ ( msgIndex, msg ) formListState =
            List.Extra.indexedFoldr
                (\index formValue acc ->
                    (if msgIndex == index then
                        form.transform msg
                            { value = formValue
                            , error =
                                List.filterMap
                                    (\( i, err ) ->
                                        if i == index then
                                            Just err

                                        else
                                            Nothing
                                    )
                                    formListState.error
                            }

                     else
                        formValue
                    )
                        :: acc
                )
                []
                formListState.value

        animate_ : State ( Int, error ) (List value) (List animation) -> List animation
        animate_ formListState =
            List.Extra.indexedFoldr
                (\index ( formValue, formAnimation ) acc ->
                    form.animate
                        { value = formValue
                        , error =
                            List.filterMap
                                (\( i, err ) ->
                                    if i == index then
                                        Just err

                                    else
                                        Nothing
                                )
                                formListState.error
                        , animation = formAnimation
                        }
                        :: acc
                )
                []
                (List.Extra.zip formListState.value formListState.animation)

        view_ : State ( Int, error ) (List value) (List animation) -> List (Element ( Int, msg ))
        view_ formListState =
            List.indexedMap
                (\index ( formValue, formAnimation ) ->
                    form.view
                        { value = formValue
                        , error =
                            List.filterMap
                                (\( i, err ) ->
                                    if i == index then
                                        Just err

                                    else
                                        Nothing
                                )
                                formListState.error
                        , animation = formAnimation
                        }
                        |> List.map (Element.map (Tuple.pair index))
                )
                (List.Extra.zip formListState.value formListState.animation)
                |> List.concat
    in
    { parse = parse_, transform = transform_, animate = animate_, view = view_ }
