module Characters where

import Control.Monad (replicateM)
import System.Random (randomRIO)

charLetters :: [Char]
charLetters = ['a' .. 'z']

charLettersCaps :: [Char]
charLettersCaps = ['A' .. 'Z']

charNumbers :: [Char]
charNumbers = ['0' .. '9']

charSpecial :: [Char]
charSpecial =
  [ '@'
  , '+'
  , '"'
  , '!'
  , '\''
  , '&'
  , '*'
  , '?'
  , '='
  , '#'
  , '<'
  , '>'
  , '-'
  , '_'
  , '|'
  , '`'
  , '{'
  , '}'
  , '['
  , ']'
  , '^'
  , '('
  , ')'
  , '$'
  , '%'
  , '\\'
  , '/'
  , ';'
  , ':'
  , '~'
  , ','
  , '.'
  ]

charCombined :: [Char]
charCombined = charLetters ++ charLettersCaps ++ charNumbers ++ charSpecial

randomChar :: [Char] -> IO Char
randomChar char_list = (char_list !!) <$> randomRIO (1, length char_list - 1)

randomString :: Int -> [Char] -> IO [Char]
randomString maxLength char_list = do
  count <- randomRIO (1, maxLength)
  replicateM count (randomChar char_list)

randomSentenceBuilder :: [Char] -> Int -> Int -> IO [Char]
randomSentenceBuilder s sentenceMaxLength wordMaxLength
  | length s < sentenceMaxLength = do
      newWord <- randomString wordMaxLength charCombined
      randomSentenceBuilder (s ++ newWord ++ [' ']) sentenceMaxLength wordMaxLength
  | otherwise = do
      last_char <- randomChar charCombined
      return (take (sentenceMaxLength - 1) s ++ [last_char])

randomSentence :: Int -> Int -> IO [Char]
randomSentence = randomSentenceBuilder ""
