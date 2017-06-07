module StringCalc exposing (..)

import List

add : String -> Int
add numbers =
  if String.contains "," numbers then
    String.split "," numbers
    |> List.map toInt
    |> List.sum
  else
    toInt numbers

toInt : String -> Int
toInt str = String.toInt str
  |> Result.toMaybe
  |> Maybe.withDefault 0
