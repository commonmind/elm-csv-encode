module TestMain exposing (suite)

{-| Tests for the csv encoder.

We test against `periodic/elm-csv` for decoding.

-}

import Csv
import Csv.Encode as E
import Expect
import Fuzz exposing (Fuzzer)
import Test exposing (..)


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
                (Ok csv)
                (Csv.parse (E.toString csv))
        )
