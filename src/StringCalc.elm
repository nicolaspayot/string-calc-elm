module StringCalc exposing (..)

import List
import Regex

separator = Regex.regex "\\n|,"

add : String -> Int
add numbers =
  splitWithSeparator numbers separator
  |> List.map toInt
  |> List.sum

splitWithSeparator numbers separator =
  Regex.split Regex.All separator numbers

-- ##### Utils #####

toInt : String -> Int
toInt str = String.toInt str
  |> Result.toMaybe
  |> Maybe.withDefault 0
