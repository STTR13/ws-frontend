module NeList exposing (..)


type alias NeList a =
    ( a, List a )


singleton : a -> NeList a
singleton a =
    ( a, [] )


append : NeList a -> NeList a -> NeList a
append ( h, list ) a =
    ( h, list ++ toList a )


appendWith : NeList a -> NeList a -> NeList a
appendWith a ( h, list ) =
    ( h, list ++ toList a )


toList : NeList a -> List a
toList ( h, list ) =
    h :: list


fromList : List a -> Maybe (NeList a)
fromList list =
    case list of
        h :: queue ->
            Just ( h, queue )

        _ ->
            Nothing


map : (a -> b) -> NeList a -> NeList b
map func ( h, list ) =
    ( func h, List.map func list )
