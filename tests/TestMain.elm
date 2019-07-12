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


module TestMain exposing (suite)

{-| Tests for the csv encoder.

We test against `periodic/elm-csv` for decoding.

-}

import Csv
import Csv.Encode as E
import Expect
import Fuzz exposing (Fuzzer)
import Test exposing (..)


{-| "Normalize" the csv. This is needed because some values encode to the
same output, so the test that expects that decode and encode are inverses
would otherwise fail. Instead of testing against the input, we test against
a normalized input.
-}
normalizeCsv : E.Csv -> E.Csv
normalizeCsv { headers, records } =
    let
        -- It's impossible to tell the difference between [] and [""] when
        -- encoded, so we normalize former before comparison.
        removeEmpties xs =
            case xs of
                [ "" ] ->
                    []

                _ ->
                    xs
    in
    { headers = removeEmpties headers
    , records = List.map removeEmpties records
    }


csvFuzzer : Fuzzer E.Csv
csvFuzzer =
    Fuzz.map2 E.Csv
        (Fuzz.list Fuzz.string)
        (Fuzz.list (Fuzz.list Fuzz.string))


suite : Test
suite =
    fuzz csvFuzzer
        "Encode-then-decode should do nothing."
        (\csv ->
            Expect.equal
                (Ok (normalizeCsv csv))
                (Csv.parse (E.toString csv)
                    |> Result.map normalizeCsv
                )
        )
