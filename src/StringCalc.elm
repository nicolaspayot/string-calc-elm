module StringCalc exposing (..)

import List

add : String -> Int
add numbers =
  splitWithSeparator numbers "\n"
  |> List.concatMap (\numberItem -> splitWithSeparator numberItem ",")
  |> List.map toInt
  |> List.sum

splitWithSeparator : String -> String -> List String
splitWithSeparator numbers separator =
  if String.contains separator numbers then
    String.split separator numbers
  else
    numbers :: []

toInt : String -> Int
toInt str = String.toInt str
  |> Result.toMaybe
  |> Maybe.withDefault 0
