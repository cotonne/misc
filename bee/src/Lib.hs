module Lib
    ( someFunc, bee
    ) where
import System.IO
import Control.Monad
	
bee :: [Char] -> Integer
bee	xs = calculate [head xs] xs
calculate :: [Char] -> [Char] -> Integer
calculate left [] = 1
calculate left (x:right) = ((value left x right) * calculate (left ++ [x]) right) `mod` 1000000007 
value left e []
  | last left == e = 1
  | otherwise = 2
value left e right 
  | last left == e && head right == e = 1
  | last left == e || head right == e = 2
  | otherwise = 3

format :: Int -> [Char] -> [Char]
format step word = "Case #" ++ (show step) ++ ": " ++ (show $ (bee word))
  
someFunc :: IO [()]
someFunc = do
  line <- getLine
  let nbOfCases = read line :: Int
  sequence $ map (\x -> getLine >>= (\y -> putStrLn $ format x y)) [1..nbOfCases]
