{-
* file: BrutForce.hs
* project: FLP 2022/2023 – funkcionální projekt: Haskell [Knapsack problem]
* autor: Jakub Komárek (xkomar33)
* description: brut force solving method
-}

module BrutForce where
import Types


brutforce :: KnapSack -> Maybe SolutionVariation
brutforce ks =  findBestSolution (solutionsEvaluation (solutionTransformation(comb (length  (items ks)) [0,1])) ks) 0  


findBestSolution :: [SolutionVariation]-> Int -> Maybe SolutionVariation
findBestSolution [] _ = Nothing
findBestSolution (x:xs) max'= 
    if (valid x)&&  (costSum x)> max'  then     
        let  sol= findBestSolution xs ( costSum x) in 
        if sol == Nothing then
            Just x
        else
            sol
    else 
        findBestSolution xs  max'

    

solutionsEvaluation :: [SolutionVariation] -> KnapSack ->  [SolutionVariation]
solutionsEvaluation [] _ =[]
solutionsEvaluation (sol:xs) ks= [solutionEval sol ks] ++ (solutionsEvaluation xs ks)

solutionEval :: SolutionVariation -> KnapSack -> SolutionVariation
solutionEval sol ks= let (totalWeight,totalCost ) =itemsEval (items ks) (itemVector sol) in 
    if totalCost >= (minCost ks) &&  totalWeight <=  (maxWeight ks) then
        sol {  weightSum =totalWeight, costSum=totalCost, valid=True }
    else
        sol {  weightSum =totalWeight, costSum=totalCost, valid=False }

itemsEval :: [Item] -> [Int] -> (Int,Int)
itemsEval [] []= (0,0)
itemsEval (x:xs) (inKnapSac:ys) = let (weightRec, costRec) = (itemsEval xs ys) in
    if inKnapSac ==1 then
        (weightRec + (weight x) , costRec + (cost x))
    else
        (weightRec, costRec)
itemsEval _ _ = brurErr


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

-- chybová hláška při prasingu
brurErr ::  a 
brurErr = error "Chyba ve vstupu"