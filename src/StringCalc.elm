module StringCalc exposing (..)

import List
import Regex

defaultSeparator = Regex.regex "\\n|,"
customSeparator = Regex.regex "^//(.)\\n"

add : String -> Int
add numbers =
  if startsWithSeparator numbers customSeparator then
    let
      separator = extractSeparator numbers customSeparator
    in
      splitWithSeparator (removeSeparatorPattern numbers) (Regex.regex separator)
      |> List.map toInt
      |> List.sum
  else
    splitWithSeparator numbers defaultSeparator
    |> List.map toInt
    |> List.sum

startsWithSeparator : String -> Regex.Regex -> Bool
startsWithSeparator numbers separator =
  Regex.contains separator numbers

extractSeparator numbers separator =
  Regex.find (Regex.AtMost 1) separator numbers
  |> List.concatMap (\match -> match.submatches)
  |> List.head
  |> unwrap
  |> unwrap

removeSeparatorPattern =
  Regex.replace (Regex.AtMost 1) customSeparator (\_ -> "")

splitWithSeparator : String -> Regex.Regex -> List String
splitWithSeparator numbers separator =
  Regex.split Regex.All separator numbers

-- ##### Utils #####

unwrap wrappedVal =
  case wrappedVal of
    Just val -> val
    Nothing -> Debug.crash "Woops!"

toInt : String -> Int
toInt str = String.toInt str
  |> Result.toMaybe
  |> Maybe.withDefault 0
