{-
* file: BrutForce.hs
* project: FLP 2022/2023 – funkcionální projekt: Haskell [Knapsack problem]
* autor: Jakub Komárek (xkomar33)
* description: brut force solving method
-}

module BrutForce where
import Types


--brutforce :: KnapSack -> [Int]
brutforce ks = solutionTransformation(comb (length  (items ks)) [0,1])

{-
solutionsEvaluation :: [SolutionVariation] ->  [SolutionVariation]
solutionsEvaluation []=[]
solutionsEvaluation (sol:xs)=
-}

solutionTransformation :: [[Int]] -> [SolutionVariation]
solutionTransformation []=[]
solutionTransformation (x:xs)= 
    let solution=SolutionVariation {itemVector= x,weightSum= -1, costSum= -1 , valid=False}in 
    [solution] ++ solutionTransformation xs


dropFirst:: [a] ->[a]
dropFirst []=[]
dropFirst (_:xs)=xs

comb :: Int -> [a] -> [[a]]
comb 0 _ = [[]]
comb n r = [i:s | i <- r, s <- comb (n-1) r]