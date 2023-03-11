{-
* file: Minimize.hs
* project: FLP 2022/2023 – funkcionální projekt: Haskell [Knapsack problem]
* autor: Jakub Komárek (xkomar33)
* description: minimaze method
-}

module Minimize where
import System.Random

--minimaze :: KnapSack -> Maybe SolutionVariation
minimaze :: IO ()
minimaze  = drawInt >>= (\x -> print $ f x)
f ::Int -> Int
f x=x
{-
generateRandomVector :: Int -> [Int]
generateRandomVector _ =[random (0, 1) newStdGen]
-}

drawInt :: IO Int
drawInt  = getStdRandom (randomR (0,1))