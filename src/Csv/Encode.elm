module Csv.Encode exposing (Csv, toBytes, toEncoder, toString)

{-| This module provides support for rendering data in csv (comma separateed
values) format. The format emitted is as desribed in [RFC4180][1].

If you want to _parse_ csv files, look at the package `lavosa/elm-csv`.

@doc toString, toBytes, toEncoder

[1]: https://tools.ietf.org/html/rfc4180

-}

import Bytes exposing (Bytes)
import Bytes.Decode as D
import Bytes.Encode as E


{-| The `Csv` type structure. This is the same as the `Csv`
type from `lavosa/elm-csv`.
-}
type alias Csv =
    { headers : List String
    , records : List (List String)
    }


{-| A bytes encoder for `Csv`s
-}
toEncoder : Csv -> E.Encoder
toEncoder { headers, records } =
    formatLines (headers :: records)


{-| Convert a `Csv` to bytes.
-}
toBytes : Csv -> Bytes
toBytes =
    toEncoder >> E.encode


{-| Convert a `Csv` to a string.
-}
toString : Csv -> String
toString csv =
    let
        bytes =
            toBytes csv
    in
    case D.decode (D.string (Bytes.width bytes)) bytes of
        Just v ->
            v

        Nothing ->
            -- This is impossible, but we have no way of convincing the
            -- type checker of this. So we just infinite loop in this
            -- case:
            toString csv


formatLines : List (List String) -> E.Encoder
formatLines =
    List.map formatRow
        >> List.intersperse crlf
        >> E.sequence


{-| Encode the string `"\r\n"`.

This is used as a line separator in the csv format (as well as other protocols
and file formats like HTTP, windows text files, ...).

`elm-format` insists on replacing the `"\r"` escape sequence with `"\u{000D}"`,
which is comparatively unreadable. A constant seemed like an easier solution
than fighting with a tool.

I(isd) submitted a patch to change the behavior, but the maintainer doesn't seem
to be terribly interested in merging it:

<https://github.com/avh4/elm-format/pull/515>

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
        >> (\s -> "\"" ++ escapeString s ++ "\"")


{-| Escape double quotes in a string.
-}
escapeString : String -> String
escapeString =
    -- In csv, double quotes are escaped by duplicating them:
    String.split "\""
        >> String.join "\"\""
