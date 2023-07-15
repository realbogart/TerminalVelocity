module Main where

import System.Random (randomRIO)
import Control.Monad (replicateM)
import System.IO

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

mainLoop :: IO ()
mainLoop = do
  c <- getChar
  if c == 'c' then 
    return ()
  else do
    putChar c
    mainLoop

startGame :: String -> IO ()
startGame str = do
  putStrLn ("Type this: " ++ str)
  putStr   "         : " 
  hFlush stdout

endGame :: IO ()
endGame = do
  putStrLn ""
  putStrLn "Game Over."

main :: IO ()
main = do
  hSetBuffering stdin NoBuffering
  str <- randomString 10 charCombined
  startGame str
  mainLoop
  endGame
  hFlush stdout
