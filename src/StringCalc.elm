module StringCalc exposing (..)

import List
import Regex

defaultSeparator = Regex.regex "\\n|,"
customSeparator = Regex.regex "^//(.+)\\n"

add : String -> Int
add numbers =
  extractNumbersList numbers
  |> List.map toInt
  |> List.sum

extractNumbersList : String -> List String
extractNumbersList numbers =
  if startsWithSeparator numbers then
    let
      separator = extractCustomSeparator numbers
      justNumbers = removeSeparatorPattern numbers
    in
      splitWithString justNumbers separator
  else
    splitWithRegex numbers

startsWithSeparator : String -> Bool
startsWithSeparator numbers =
  Regex.contains customSeparator numbers

extractCustomSeparator : String -> String
extractCustomSeparator numbers =
  Regex.find (Regex.AtMost 1) customSeparator numbers
  |> List.concatMap (\match -> match.submatches)
  |> List.head
  |> unwrap
  |> unwrap

removeSeparatorPattern : String -> String
removeSeparatorPattern =
  Regex.replace (Regex.AtMost 1) customSeparator (\_ -> "")

splitWithString : String -> String -> List String
splitWithString numbers separator =
  String.split separator numbers

splitWithRegex : String -> List String
splitWithRegex numbers =
  Regex.split Regex.All defaultSeparator numbers

-- ##### Utils #####

unwrap : Maybe.Maybe a -> a
unwrap wrappedVal =
  case wrappedVal of
    Just val -> val
    Nothing -> Debug.crash "Woops!"

toInt : String -> Int
toInt str = String.toInt str
  |> Result.toMaybe
  |> Maybe.withDefault 0
