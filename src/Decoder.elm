module Decoder exposing (..)

import Json.Decode as Decode
import Json.Encode as Encode
import Json.Decode.Pipeline as DecodePipe


type alias Data =
    { data : List Result
    }


type alias Result =
    { meaningList : List Meaning
    }


type alias Meaning =
    { text : String
    }


dataDecoder =
    DecodePipe.decode Data
        |> DecodePipe.optional "tuc" (Decode.list resultDecoder) []


resultDecoder =
    DecodePipe.decode Result
        |> DecodePipe.optional "meanings" (Decode.list meaningDecoder) []


meaningDecoder =
    DecodePipe.decode Meaning
        |> DecodePipe.optional "text" Decode.string ""
