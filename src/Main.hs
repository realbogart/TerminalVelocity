module Main where

import System.Random (randomRIO)
import Control.Monad (replicateM)

charLetters :: [Char]
charLetters = ['a'..'z']

charNumbers :: [Char]
charNumbers = ['0'..'9']

charSpecial :: [Char]
charSpecial = ['@', '+', '"']

charCombined :: [Char]
charCombined = charLetters ++ charNumbers ++ charSpecial

randomChar :: [Char] -> IO Char
randomChar char_list = (char_list !!) <$> randomRIO (1, length char_list - 1)

randomString :: Int -> [Char] -> IO [Char]
randomString count char_list = replicateM count (randomChar char_list)

main :: IO ()
main = randomString 10 charCombined >>= print

