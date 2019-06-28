module FormatCsv exposing (Csv, crlf, encodeBytes, toBytes)

{-| This module provides support for rendering data in csv format.
-}

import Bytes exposing (Bytes)
import Bytes.Encode as E


{-| The `Csv` type structure. This is the same as the `Csv`
type from "lavosa/elm-csv".
-}
type alias Csv =
    { headers : List String
    , records : List (List String)
    }


{-| Render a `Csv` to a csv file, per rfc4180.
-}
encodeBytes : Csv -> E.Encoder
encodeBytes { headers, records } =
    formatLines (headers :: records)


toBytes : Csv -> Bytes
toBytes =
    encodeBytes >> E.encode


formatLines : List (List String) -> E.Encoder
formatLines =
    List.map formatRow
        >> List.intersperse crlf
        >> E.sequence


{-| Encode the string `"\r\n"`.

This is used as a line separator in the csv format, as well as other protocols
and file formats (HTTP, windows text files, ...).

Originally, we defined a constant string for this because elm-format insists on
replacing the `"\r"` escape sequence with `"\u{000D}"`, which is comparatively
unreadable. The constant seemed an easier solution that fighting with a tool.

I(isd) submitted a patch to change the behavior, but the maintainer doesn't seem
to be terribly interested in merging it:

<https://github.com/avh4/elm-format/pull/515>

Now that we've switched over to using elm/bytes, it's an encoder, not a string.

-}
crlf : E.Encoder
crlf =
    E.string "\u{000D}\n"


formatRow : List String -> E.Encoder
formatRow =
    List.map formatField
        >> List.intersperse ","
        >> List.map E.string
        >> E.sequence


formatField : String -> String
formatField =
    String.toList
        >> List.map
            (\c ->
                case c of
                    '"' ->
                        "\"\""

                    _ ->
                        String.fromChar c
            )
        >> String.concat
        >> (\s -> "\"" ++ s ++ "\"")
