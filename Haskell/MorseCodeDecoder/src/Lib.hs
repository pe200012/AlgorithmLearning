module Lib where

import           Control.Arrow   (Arrow ((&&&)), second)
import           Data.Function   ((&))
import           Data.List       (dropWhileEnd, unfoldr)
import           Data.List.Split (splitOn)
import           Data.Map.Strict (fromList, Map, (!))
import           Prelude         hiding (lex)

morseCodes :: Map String String
morseCodes = fromList [
  (".-", "A"),
  ("-...", "B"),
  ("-.-.", "C"),
  ("-..", "D"),
  (".", "E"),
  ("..-.", "F"),
  ("--.", "G"),
  ("....", "H"),
  ("..", "I"),
  (".---", "J"),
  ("-.-", "K"),
  (".-..", "L"),
  ("--", "M"),
  ("-.", "N"),
  ("---", "O"),
  (".--.", "P"),
  ("--.-", "Q"),
  (".-.", "R"),
  ("...", "S"),
  ("-", "T"),
  ("..-", "U"),
  ("...-", "V"),
  (".--", "W"),
  ("-..-", "X"),
  ("-.--", "Y"),
  ("--..", "Z")
  ]

someFunc :: IO ()
someFunc = putStrLn "someFunc"

lex :: String -> [[String]]
lex str = go str [] [] ""
  where
    go :: String -> [[String]] -> [String] -> String -> [[String]]
    go "" sentence word ch = reverse ((reverse (reverse ch : word)) : sentence)
    go (' ':' ':' ':rest) sentence word ch =
      go rest (reverse (reverse ch : word) : sentence) [] ""
    go (' ':rest) sentence word ch = go rest sentence (reverse ch : word) ""
    go (c:rest) sentence word ch = go rest sentence word (c : ch)

decodeBits :: [Char] -> [[String]]
decodeBits str =
   map (map (map draw))
   . map (map (delimiter id))
   . map (delimiter (*3))
   . delimiter (*7)
   . ((.) . dropWhile <*> dropWhileEnd) (== '0')
    $ str
  where
    draw ones
      | length ones == n = '.'
      | otherwise = '-'
    delimiter = splitOn . flip replicate '0' . (&) n
    n = minimum . fmap length . filter (not . null) . splitOn "1" $ str

decodeMorse :: [[String]] -> String
decodeMorse = unwords . fmap (concatMap (morseCodes !))
