{-
* file: BrutForce.hs
* project: FLP 2022/2023 – funkcionální projekt: Haskell [Knapsack problem]
* autor: Jakub Komárek (xkomar33)
* description: brut force solving method
-}
module BrutForce where
    
import Types

-- hlavní funkce, vygreneruje všechny možné kombinace řešení, které následně vyhodnotí a pokud jsou nalezena řešení,
-- vrací nejlepší znich 
brutforce :: KnapSack -> Maybe SolutionVariation
brutforce ks =  findSolution (solutionsEvaluation (solutionTransformation(comb (length  (items ks)) [0,1])) ks) 

-- provede vyhledání nejlepšího řešení, vrací Nothing pokud žádné řešení neexistuje
-- tato funkce není užita - lze použít nahrazením findSolution v brutforce módě
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

-- nalezne první nalezené řešení, pokud neexistuje vrací nothing
findSolution :: [SolutionVariation] -> Maybe SolutionVariation
findSolution [] = Nothing
findSolution (x:xs) = 
    if (valid x)then     
        Just x
    else 
        findSolution xs

-- hlavní funkce na vyhodnocení všech řešení
solutionsEvaluation :: [SolutionVariation] -> KnapSack ->  [SolutionVariation]
solutionsEvaluation [] _ =[]
solutionsEvaluation (sol:xs) ks= [solutionEval sol ks] ++ (solutionsEvaluation xs ks)

-- provádí vyhodnocení jednoho konkrétního řešení
solutionEval :: SolutionVariation -> KnapSack -> SolutionVariation
solutionEval sol ks= let (totalWeight,totalCost ) =itemsEval (items ks) (itemVector sol) in 
    if totalCost >= (minCost ks) &&  totalWeight <=  (maxWeight ks) then
        sol {  weightSum =totalWeight, costSum=totalCost, valid=True }
    else
        if totalWeight <=  (maxWeight ks) then
            sol {  weightSum =totalWeight, costSum=totalCost, valid=False }
        else 
            sol {  weightSum =totalWeight, costSum=0, valid=False }

-- vrací součty všech váh a cen všech itemů v knapsaku. Itemy které patři do řešení rozhoduje  vektor
itemsEval :: [Item] -> [Int] -> (Int,Int)
itemsEval [] []= (0,0)
itemsEval (x:xs) (inKnapSac:ys) = let (weightRec, costRec) = (itemsEval xs ys) in
    if inKnapSac ==1 then
        (weightRec + (weight x) , costRec + (cost x))
    else
        (weightRec, costRec)
itemsEval _ _ = brurErr

-- vytvoří všechna řešení z vektorů - tvorba struktury SolutionVariation
solutionTransformation :: [[Int]] -> [SolutionVariation]
solutionTransformation []=[]
solutionTransformation (x:xs)= 
    let solution=SolutionVariation {itemVector= x,weightSum= -1, costSum= -1 , valid=False}in 
    [solution] ++ solutionTransformation xs

-- vytvoří všechny kombinace vektorů o délce n
comb :: Int -> [a] -> [[a]]
comb 0 _ = [[]]
comb n r = [i:s | i <- r, s <- comb (n-1) r]

-- chybová hláška při prasingu
brurErr ::  a 
brurErr = error "Chyba ve vstupu"