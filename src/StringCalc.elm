module StringCalc exposing (..)

add : String -> Int
add numbers = String.toInt numbers
  |> Result.toMaybe
  |> Maybe.withDefault 0
