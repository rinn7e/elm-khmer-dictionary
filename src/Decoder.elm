module Decoder exposing (..)

import Json.Decode as Decode
import Json.Encode as Encode
import Json.Decode.Pipeline as DecodePipe


type alias Data =
    { data : List Tuc
    }


type alias Tuc =
    { meaning : List Meaning
    }


type alias Meaning =
    { text : String
    }


dataDecoder =
    DecodePipe.decode Data
        |> DecodePipe.optional "tuc" (Decode.list tucDecoder) []


tucDecoder =
    DecodePipe.decode Tuc
        |> DecodePipe.optional "meaning" (Decode.list meaningDecoder) []


meaningDecoder =
    DecodePipe.decode Meaning
        |> DecodePipe.optional "text" Decode.string ""
