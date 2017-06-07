module StringCalcTests exposing (..)

import Test exposing (..)
import Expect
import Fuzz exposing (Fuzzer)

import StringCalc

oneNumber : Fuzzer String
oneNumber =
  Fuzz.map
    (\randomInt -> toString randomInt)
    (Fuzz.intRange 0 100)

toInt : String -> Int
toInt str = String.toInt str
  |> Result.toMaybe
  |> Maybe.withDefault 0

suite : Test
suite =
    describe "The string calculator"
      [ test "should return 0 with empty string" <|\() ->
          Expect.equal (StringCalc.add "") 0

      , test "should return 1 with '1' as string" <|\() ->
          Expect.equal (StringCalc.add "1") 1

      , fuzz (oneNumber) "should return number n with 'n' as string" <|\numbers ->
          Expect.equal (StringCalc.add numbers) (toInt numbers)
      ]
