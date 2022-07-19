-- Do not manually edit this file, it was auto-generated by dillonkearns/elm-graphql
-- https://github.com/dillonkearns/elm-graphql


module API.Scalar exposing (Codecs, Id(..), Time(..), defaultCodecs, defineCodecs, unwrapCodecs, unwrapEncoder)

import Graphql.Codec exposing (Codec)
import Graphql.Internal.Builder.Object as Object
import Graphql.Internal.Encode
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode


type Id
    = Id String


type Time
    = Time String


defineCodecs :
    { codecId : Codec valueId
    , codecTime : Codec valueTime
    }
    -> Codecs valueId valueTime
defineCodecs definitions =
    Codecs definitions


unwrapCodecs :
    Codecs valueId valueTime
    ->
        { codecId : Codec valueId
        , codecTime : Codec valueTime
        }
unwrapCodecs (Codecs unwrappedCodecs) =
    unwrappedCodecs


unwrapEncoder :
    (RawCodecs valueId valueTime -> Codec getterValue)
    -> Codecs valueId valueTime
    -> getterValue
    -> Graphql.Internal.Encode.Value
unwrapEncoder getter (Codecs unwrappedCodecs) =
    (unwrappedCodecs |> getter |> .encoder) >> Graphql.Internal.Encode.fromJson


type Codecs valueId valueTime
    = Codecs (RawCodecs valueId valueTime)


type alias RawCodecs valueId valueTime =
    { codecId : Codec valueId
    , codecTime : Codec valueTime
    }


defaultCodecs : RawCodecs Id Time
defaultCodecs =
    { codecId =
        { encoder = \(Id raw) -> Encode.string raw
        , decoder = Object.scalarDecoder |> Decode.map Id
        }
    , codecTime =
        { encoder = \(Time raw) -> Encode.string raw
        , decoder = Object.scalarDecoder |> Decode.map Time
        }
    }
