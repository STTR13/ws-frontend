module Element.Extra exposing (..)

import Element
import Html.Attributes


noneAttribute : Element.Attribute msg
noneAttribute =
    Element.htmlAttribute (Html.Attributes.classList [])
