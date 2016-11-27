module Lib
    ( someFunc, robot
    ) where

import Data.Bits                                as D
import Control.Monad
import Control.Parallel (par, pseq)
import qualified Data.Array.Accelerate          as A
import Data.Array.Accelerate.Data.Bits          as B
import qualified Data.Array.Accelerate.CUDA     as CUDA


calculateK :: [Int] -> Int
calculateK x = foldl D.xor (head x) (tail x)

-- (Hope to be) Efficient version with CUDA - unfinished
-- With length = 1000 => 10^12 cases [a,b,c,d]
-- a xor b = left ~ 1million, c xor d = right ~ 1 million
-- Missing step = left xor right
-- In order to optimise parallel calculation and due to limitation
-- of speed with PCI-E, we have to send r to the card
-- And build everything there
-- aDim0     = [ 1, 2, 3, 4]
-- replicate = [ 1, 2, 3, 4,
--               1, 2, 3, 4,
--               1, 2, 3, 4,
--               1, 2, 3, 4]
-- reshape   = [ 1, 2, 3, 4, 1, ==> shift
--               2, 3, 4, 1, 2,
--               3, 4, 1, 2, 3,
--               4, 1, 2, 3, 4]
-- transpose = [ 1, 2, 3, 4,
--               2, 3, 4, 1,
--               3, 4, 1, 2,
--               4, 1, 2, 3,
--               1, 2, 3, 4]
-- reshape'  = [ 1, 2, 3, 4, ==> Remove last line
--               2, 3, 4, 1,
--               3, 4, 1, 2,
--               4, 1, 2, 3]
-- aDim1     = [ 5, 6, 7, 8,
--               5, 6, 7, 8,
--               5, 6, 7, 8,
--               5, 6, 7, 8]
-- zipWith + = [ 6,  8, 10, 12,
--               7,  9, 11,  9,
--               8, 10,  8, 10,
--               9,  7,  9, 11]
-- reshape   = [6,8,10,12,7,9,11,9,8,10,8,10,9,7,9,11] ==> flatten the list
-- only have to count matches
robotAcc :: Int -> [[Int]] -> Int
robotAcc k r = length [1 | x <- left, y <- right, (x `D.xor` y) == k]
  where
    left  = A.toList ( (CUDA.run $ A.reshape (A.index1 lprod) $ A.zipWith (B.xor) matShf0 matShf1) :: A.Vector Int)
    right = A.toList ( (CUDA.run $ A.reshape (A.index1 lprod) $ A.zipWith (B.xor) matShf2 matShf3) :: A.Vector Int)
    matShf0 = ((A.reshape (A.index2 l l) $ A.transpose $ A.reshape  (A.index2 l (lp1)) rep0) :: A.Acc (A.Array A.DIM2 Int))
    matShf1 = (A.replicate (A.lift (A.Z A.:. (len::Int) A.:. A.All)) aDim1)
    matShf2 = ((A.reshape (A.index2 l l) $ A.transpose $ A.reshape  (A.index2 l (lp1)) rep2) :: A.Acc (A.Array A.DIM2 Int))
    matShf3 = (A.replicate (A.lift (A.Z A.:. (len::Int) A.:. A.All)) aDim3)
    rep0 = A.replicate (A.lift (A.Z A.:. ((len+1)::Int) A.:. A.All ) ) aDim0
    rep2 = A.replicate (A.lift (A.Z A.:. ((len+1)::Int) A.:. A.All ) ) aDim2
    aDim0 = A.use (A.fromList (A.Z A.:. len) (r!!0) :: A.Array A.DIM1 Int)
    aDim1 = A.use (A.fromList (A.Z A.:. len) (r!!1) :: A.Array A.DIM1 Int)
    aDim2 = A.use (A.fromList (A.Z A.:. len) (r!!2) :: A.Array A.DIM1 Int)
    aDim3 = A.use (A.fromList (A.Z A.:. len) (r!!3) :: A.Array A.DIM1 Int)
    len = (length $ r!!0)
    l = A.constant len
    lp1 = A.constant (len+1)
    lprod = A.constant (len*len)
    ke = A.constant k
    k0 = A.constant 0 :: A.Exp Int

-- Parallel version
parRobot :: Int -> [[Int]] -> Int -> Int
parRobot _ ([]:_) _ = 0
parRobot k r chunkSize = a `par` b `pseq` (b + a)
  where 
   firstRobot = head r
   (left, right) = splitAt chunkSize firstRobot
   one   = left  : tail r
   other = right : tail r
   a = robot k one
   b = robot k other

-- Version with a list-comprehension, reduce memory consumption
robotListC :: Int -> [[Int]] -> Int
robotListC k r = length [1|r0 <- r!!0, r1 <- r!!1, r2 <- r!!2, r3 <- r!!3, calculateK [r0, r1, r2, r3, k] == 0]

-- Naive version
-- Huge memory consumption due to sequence
-- All cases are created before
robot :: Int -> [[Int]] -> Int
robot k r = length $ filter (==k) $ map calculateK $ sequence r

format :: Int -> Int -> [[Int]] -> [Char]
-- format step k r = "Case #" ++ (show step) ++ ": " ++ (show $ (parRobot k r 5))
format step k r = "Case #" ++ (show step) ++ ": " ++ (show $ (robotAcc k r))


readOneCase x = do
  input_line <- getLine
  let (n, k) = readNandK input_line
  robots_line <- replicateM 4 $ do getLine
  let robots = map readOneRobotLine robots_line
  putStrLn $ format x k robots

readNandK s = (read (w!!0) :: Int, read (w!!1) :: Int)
  where w = words s

readOneRobotLine = map (\x -> read x :: Int) . words 

someFunc :: IO [()]
someFunc = do
  line <- getLine
  let nbOfCases = read line :: Int
  sequence $ map (\x -> readOneCase x) [1..nbOfCases]
  
-- 