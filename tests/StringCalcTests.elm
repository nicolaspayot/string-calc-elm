module StringCalcTests exposing (..)

import Test exposing (..)
import Expect

import StringCalc

suite : Test
suite =
    describe "The string calculator"
      [ test "should return 0 with empty string" <|\() ->
          Expect.equal (StringCalc.add "") 0

      , test "should return 1 with '1' as string" <|\() ->
          Expect.equal (StringCalc.add "1") 1

      , test "should return 1234 with '1234' as string" <|\() ->
          Expect.equal (StringCalc.add "1234") 1234

      , test "should return 3 with '1,2' as string" <|\() ->
          Expect.equal (StringCalc.add "1,2") 3

      , test "should return 45 with '1,2,3,4,5,6,7,8,9' as string" <|\() ->
          Expect.equal (StringCalc.add "1,2,3,4,5,6,7,8,9") 45

      , test "should return 6 with '1\n2,3' as string" <|\() ->
          Expect.equal (StringCalc.add "1\n2,3") 6

      , test "should return 3 with '//;\n1;2' as string" <|\() ->
          Expect.equal (StringCalc.add "//;\n1;2") 3
      ]
