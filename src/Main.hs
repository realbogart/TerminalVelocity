module Main where

import System.Random (randomRIO)
import Control.Monad (replicateM)
import System.IO
import Data.Char

charLetters :: [Char]
charLetters = ['a'..'z']

charLettersCaps :: [Char]
charLettersCaps = ['A'..'Z']

charNumbers :: [Char]
charNumbers = ['0'..'9']

charSpecial :: [Char]
charSpecial = [ '@', '+', '"', '!', '\'', '&', '*', '?', '=', '#',
                '<', '>', '-', '_', '|', '`', '{', '}', '[', ']', 
                '^', '(', ')', '$', '%', '\\', '/', ';', ':', '~',
                ',', '.']

charCombined :: [Char]
charCombined = charLetters ++ charLettersCaps ++ charNumbers ++ charSpecial

randomChar :: [Char] -> IO Char
randomChar char_list = (char_list !!) <$> randomRIO (1, length char_list - 1)

randomString :: Int -> [Char] -> IO [Char]
randomString count char_list = replicateM count (randomChar char_list)

roundStart :: String -> IO ()
roundStart str = do
  putStrLn ("Type this: " ++ str)
  putStr   "         : " 
  hFlush stdout

roundLoop :: String -> IO (Bool, String)
roundLoop "" = return (True, "")
roundLoop (current:remaining) = do
  input <- getChar
  if (ord input == 27) || (input /= current) then 
    return (False, current:remaining)
  else do
    putChar input
    hFlush stdout
    roundLoop remaining

roundSucceded :: IO ()
roundSucceded = do
  putStrLn ""
  putStrLn "Failed."

roundFailed :: IO ()
roundFailed = do
  putStrLn ""
  putStrLn "Success!"

main :: IO ()
main = do
  hSetBuffering stdin NoBuffering
  hSetEcho stdin False
  str <- randomString 10 charCombined
  roundStart str
  (result, remaining) <- roundLoop str
  if result then do
    roundFailed
  else
    roundSucceded
  hFlush stdout
