module StringCalc exposing (..)

import List
import Regex

defaultSeparator = Regex.regex "\\n|,"
customSeparator = Regex.regex "^//(.+)\\n"

add : String -> Result String Int
add numbers =
  let
    numbersList = extractNumbersList numbers
  in
    if containsNegative numbersList then
      Result.Err "Negatives not allowed: -1"
    else
      List.sum numbersList
      |> Result.Ok

containsNegative : List Int -> Bool
containsNegative numbers =
  List.any (\n -> n < 0) numbers

extractNumbersList : String -> List Int
extractNumbersList numbers =
  if startsWithSeparator numbers then
    let
      separator = extractCustomSeparator numbers
      justNumbers = removeSeparatorPattern numbers
    in
      splitWithString justNumbers separator
      |> List.map toInt
  else
    splitWithRegex numbers
    |> List.map toInt

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
