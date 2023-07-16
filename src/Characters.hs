module Characters where

import Control.Monad (replicateM)
import System.Random (randomRIO)
import Data.List (intercalate)

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

randomSentence :: Int -> Int -> IO [Char]
randomSentence wordCount wordMaxLength = do 
  sentence <- replicateM wordCount (randomString wordMaxLength charCombined)
  return (intercalate [' '] sentence)
