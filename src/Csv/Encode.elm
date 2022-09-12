module Csv.Encode exposing
    ( Csv, toString, toBytes, toEncoder
    , toBytesWith, toEncoderWith, toStringWith
    )

{-| This module provides support for rendering data in csv (comma separateed
values) format. The format emitted is as described in [RFC4180][1].
If you want to _parse_ csv files, look at the package `periodic/elm-csv`;
this package is designed to work well with it.

@docs Csv, toString, toBytes, toEncoder
@docs [1]: https://tools.ietf.org/html/rfc4180

-}

-- Copyright (C) 2019 CommonMind LLC
--
-- This program is free software: you can redistribute it and/or modify
-- it under the terms of the GNU Affero General Public License as published by
-- the Free Software Foundation, either version 3 of the License, or
-- (at your option) any later version.
--
-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU Affero General Public License for more details.
--
-- You should have received a copy of the GNU Affero General Public License
-- along with this program.  If not, see <https://www.gnu.org/licenses/>.

import Bytes exposing (Bytes)
import Bytes.Decode as D
import Bytes.Encode as E


{-| The `Csv` type structure. This is the same as the `Csv`
type from `periodic/elm-csv`.
-}
type alias Csv =
    { headers : List String
    , records : List (List String)
    }


type alias Settings =
    { quoted : Bool
    , delimiter : String
    }


defaultEncodeSettings : Settings
defaultEncodeSettings =
    { quoted = True, delimiter = "," }


{-| A bytes encoder for `Csv`s.
-}
toEncoder : Csv -> E.Encoder
toEncoder =
    toEncoderWith defaultEncodeSettings


{-| A bytes encoder for `Csv`s with `Settings`.
-}
toEncoderWith : Settings -> Csv -> E.Encoder
toEncoderWith setts { headers, records } =
    E.sequence
        [ formatLines setts (headers :: records)
        , crlf
        ]


{-| Convert a `Csv` to bytes.
-}
toBytes : Csv -> Bytes
toBytes =
    toBytesWith defaultEncodeSettings


{-| Convert a `Csv` to bytes with `Settings`.
-}
toBytesWith : Settings -> Csv -> Bytes
toBytesWith setts =
    toEncoderWith setts >> E.encode


{-| Convert a `Csv` to a string.
-}
toString : Csv -> String
toString =
    toStringWith defaultEncodeSettings


{-| Convert a `Csv` to a string with `Settings`.
-}
toStringWith : Settings -> Csv -> String
toStringWith setts csv =
    let
        bytes =
            toBytesWith setts csv
    in
    case D.decode (D.string (Bytes.width bytes)) bytes of
        Just v ->
            v

        Nothing ->
            -- This is impossible, but we have no way of convincing the
            -- type checker of this. So we just infinite loop in this
            -- case:
            toString csv


formatLines : Settings -> List (List String) -> E.Encoder
formatLines setts =
    List.map (formatRow setts)
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


formatRow : Settings -> List String -> E.Encoder
formatRow ({ delimiter } as setts) =
    List.map (formatField setts)
        >> List.intersperse delimiter
        >> List.map E.string
        >> E.sequence


formatField : Settings -> String -> String
formatField { quoted, delimiter } s =
    if quoted || String.contains delimiter s then
        "\"" ++ escapeString s ++ "\""

    else
        escapeString s


{-| Escape double quotes in a string.
-}
escapeString : String -> String
escapeString =
    -- In csv, double quotes are escaped by duplicating them:
    String.split "\""
        >> String.join "\"\""
