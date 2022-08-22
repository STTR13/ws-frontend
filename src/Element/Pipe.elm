module Element.Pipe exposing (..)

import Element exposing (Element)


type alias Builder msg =
    { orientation : Orientation
    , attributes : List (Element.Attribute msg)
    , elements : List (Element msg)
    }


type Orientation
    = Row
    | WrappedRow
    | Column



-- ██╗███╗   ██╗██╗████████╗
-- ██║████╗  ██║██║╚══██╔══╝
-- ██║██╔██╗ ██║██║   ██║
-- ██║██║╚██╗██║██║   ██║
-- ██║██║ ╚████║██║   ██║
-- ╚═╝╚═╝  ╚═══╝╚═╝   ╚═╝


empty : Orientation -> Builder msg
empty orientation =
    { orientation = orientation, attributes = [], elements = [] }


{-| defaults to Column
-}
singleton : Element msg -> Builder msg
singleton el =
    { orientation = Column, attributes = [], elements = [ el ] }



-- ███████╗██╗   ██╗ ██████╗ ██╗    ██╗   ██╗███████╗
-- ██╔════╝██║   ██║██╔═══██╗██║    ██║   ██║██╔════╝
-- █████╗  ██║   ██║██║   ██║██║    ██║   ██║█████╗
-- ██╔══╝  ╚██╗ ██╔╝██║   ██║██║    ╚██╗ ██╔╝██╔══╝
-- ███████╗ ╚████╔╝ ╚██████╔╝███████╗╚████╔╝ ███████╗
-- ╚══════╝  ╚═══╝   ╚═════╝ ╚══════╝ ╚═══╝  ╚══════╝


setOrientation : Orientation -> Builder msg -> Builder msg
setOrientation newOrientation builder =
    { builder | orientation = newOrientation }


addAttribute : Element.Attribute msg -> Builder msg -> Builder msg
addAttribute attr builder =
    { builder | attributes = builder.attributes ++ [ attr ] }


append : Element msg -> Builder msg -> Builder msg
append elem builder =
    { builder | elements = builder.elements ++ [ elem ] }


map : (a -> b) -> Builder a -> Builder b
map func builder =
    { orientation = builder.orientation
    , attributes = List.map (Element.mapAttribute func) builder.attributes
    , elements = List.map (Element.map func) builder.elements
    }



-- ██╗   ██╗██╗███████╗██╗    ██╗
-- ██║   ██║██║██╔════╝██║    ██║
-- ██║   ██║██║█████╗  ██║ █╗ ██║
-- ╚██╗ ██╔╝██║██╔══╝  ██║███╗██║
--  ╚████╔╝ ██║███████╗╚███╔███╔╝
--   ╚═══╝  ╚═╝╚══════╝ ╚══╝╚══╝


toElement : Builder msg -> Element msg
toElement builder =
    case builder.orientation of
        Row ->
            Element.row builder.attributes builder.elements

        WrappedRow ->
            Element.wrappedRow builder.attributes builder.elements

        Column ->
            Element.column builder.attributes builder.elements
