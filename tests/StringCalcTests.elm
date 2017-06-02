module StringCalcTests exposing (..)

import Test exposing (..)
import Expect
import Fuzz exposing (Fuzzer)

import StringCalc

oneNumber : Fuzzer Int
oneNumber = Fuzz.intRange 0 100

suite : Test
suite =
    describe "The string calculator"
      [ test "should return 0 with empty string" <|\() ->
          Expect.equal (StringCalc.add "") 0

      , test "should return 1 with '1' as string" <|\() ->
          Expect.equal (StringCalc.add "1") 1

      , fuzz (oneNumber) "should return number n with 'n' as string" <|\numbers ->
          toString numbers
            |> StringCalc.add
            |> Expect.equal numbers
      ]
