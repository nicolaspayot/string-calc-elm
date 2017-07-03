module StringCalc exposing (..)

import List
import Regex

defaultSeparator = Regex.regex "\\n|,"
customSeparator = Regex.regex "^//(.+)\\n"

add : String -> Result String Int
add numbers =
  let
    numbersList = numbers
    |> extractNumbers
    |> numbersToInt
  in
    if containsNegative numbersList then
      extractNegatives numbersList
      |> String.join ", "
      |> concatNegatives
    else
      List.sum numbersList
      |> Result.Ok

extractNumbers : String -> List String
extractNumbers numbers =
  if startsWithSeparator numbers then
    let
      separator = extractCustomSeparator numbers
      justNumbers = removeSeparatorPattern numbers
    in
      splitWithString justNumbers separator
  else
    splitWithRegex numbers

numbersToInt : List String -> List Int
numbersToInt numbers =
  List.map toInt numbers

containsNegative : List Int -> Bool
containsNegative numbers =
  List.any (\n -> n < 0) numbers

extractNegatives : List Int -> List String
extractNegatives numbers =
  List.filter (\n -> n < 0) numbers
  |> List.map toString

concatNegatives : String -> Result String value
concatNegatives negatives =
  String.concat ["Negatives not allowed: ", negatives]
  |> Result.Err

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
