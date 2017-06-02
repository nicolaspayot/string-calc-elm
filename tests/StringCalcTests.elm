module StringCalcTests exposing (..)

import Test exposing (..)
import Expect

import StringCalc

suite : Test
suite =
    describe "The string calculator"
      [ test "should return 0 with empty string" <|\() ->
        Expect.equal (StringCalc.add "") 0
      ]
