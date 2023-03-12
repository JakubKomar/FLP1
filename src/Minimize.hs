{-
* file: Minimize.hs
* project: FLP 2022/2023 – funkcionální projekt: Haskell [Knapsack problem]
* autor: Jakub Komárek (xkomar33)
* description: minimaze method
-}

module Minimize where
import System.Random
import Types
import BrutForce
import Debug.Trace
-- minimaze :: KnapSack -> IO ()
minimaze ks = do
    gen <- getStdGen 
    let rs = MiniStr {boolRands= (randomRs (0, 1 :: Int) gen ), intRands=randoms  gen,doubleRands= (randomRs(0, 1 :: Double) gen ), intCnt=  0} 

    return $ startMinimaze ks rs 


-- startMinimaze :: KnapSack -> MiniStr -> Int
startMinimaze ks rs= 
    let (sols, newRs)= genereteSolutions 8 ks rs in
    begginGenAlg  newRs  ks sols

begginGenAlg :: MiniStr -> KnapSack ->[SolutionVariation] -> Maybe SolutionVariation
begginGenAlg rs ks sols= 
    let (newRs,sufleSols)=shuffle rs sols in
    let x=solutionsEvaluation sufleSols ks in 
    let y=findSolution x in
    if y/=Nothing then
        y
    else 
        let (newRs1, fightedSol) = fightSolutions newRs ks x in
        let (newRs2, fightedSufleSol) = shuffle newRs1 fightedSol  in
        let (newRs3, randInt) = tosInt newRs2 in
        let (newRs4, newGeneration) = createChilds newRs3 randInt fightedSufleSol in
        (trace $ show x)
        begginGenAlg newRs4 ks newGeneration


mutateVector :: MiniStr -> [Int] -> (MiniStr, [Int])
mutateVector rs []= (rs,[])
mutateVector rs (x:xs) =
    let (newGen,randFactor) = tosChance rs 0.2 in
    let (newGen2,vectRest)= mutateVector newGen xs in
    if randFactor then
        if x==0 then
            (newGen2,[1] ++ vectRest )
        else
            (newGen2,[0] ++ vectRest )
    else
        (newGen2,[x] ++ vectRest )


createChilds :: MiniStr -> Int -> [SolutionVariation] -> (MiniStr,[SolutionVariation]) 
createChilds rs _ []=(rs,[])
createChilds rs rand (x:y:xs) =
    let splitPos = normalizeRndInt rand  (length (itemVector x)) in 
    let (ax,ay)=splAt splitPos (itemVector x) in
    let (bx,by)=splAt splitPos (itemVector y) in 
    let (newGen,mutatedA) = mutateVector rs (ax++by) in
    let (newGen2,mutatedB) = mutateVector newGen (bx++ay) in
    let (newGen3,rest) = createChilds newGen2 rand xs in
    (newGen3,[x,y,SolutionVariation {itemVector= mutatedA,valid=False},SolutionVariation {itemVector= mutatedB,valid=False}] ++ rest)

splAt :: Int -> [a]->([a],[a])
splAt _ []=([],[])
splAt 0 x = ([],x)
splAt i (x:xs) = 
    let (a,b)= splAt (i-1) xs in
    ([x]++a, b)

normalizeRndInt :: Int ->Int ->Int 
normalizeRndInt x upperBount= (absVal x) `mod` upperBount

fightSolutions ::  MiniStr ->KnapSack-> [SolutionVariation] -> ( MiniStr ,[SolutionVariation] )
fightSolutions rs _ []= (rs,[])
fightSolutions rs ks (x:y:xs) = 
    let (newGen,rest)=fightSolutions rs ks xs in
    if  costSum x < costSum y then
        (newGen,[y]++ rest)
    else if (costSum x)==0 && (costSum y) ==0 then
        let (newSol,newGen2)= genereteSolutions 1 ks newGen in
        --(trace $ show newSol)
        (newGen2,newSol ++ rest)
    else
        (newGen,[x]++ rest)
-- fightSolutions rs _ x  = (rs,[])

findSolution :: [SolutionVariation] -> Maybe SolutionVariation
findSolution [] = Nothing
findSolution (x:xs) = 
    if (valid x)then     
        Just x
    else 
        findSolution xs

shuffle  :: MiniStr ->[a] -> (MiniStr,[a] )
shuffle rs []= (rs,[])
shuffle rs (x:xs)=
    let (newRs,int)=tosInt (rs) in
    let absInt =  absVal int in
    let pos =  absInt `mod` ((length xs )+1 ) in
    if pos == 0 then
        let (newRs2,rest)=shuffle newRs xs in
        (newRs2,[x] ++ rest)
    else 
        let a = xs !! (pos-1) in      
        let (newRs2,rest)=shuffle newRs ((removeN (pos-1) xs )++[x]) in
        (newRs2,[a] ++ rest)
       
removeN :: Int -> [a] ->[a]
removeN _ []=[]
removeN i (x:xs)=
    if i ==0 then
        removeN (i-1) xs 
    else
        [x]++ (removeN (i-1) xs )


absVal :: Int ->Int
absVal x = if x>=0 then
            x
        else
            (-x) 


genereteSolutions:: Int ->KnapSack -> MiniStr -> ([SolutionVariation],MiniStr)
genereteSolutions i ks rs= 
    let (newRs,vectors) = tosNVectors rs i $ length (items ks) in
    let sols= solutionTransformation vectors in
    (sols,newRs)

tosNVectors :: MiniStr -> Int -> Int -> (MiniStr,[[Int]]) 
tosNVectors rs 0 _= (rs,[])
tosNVectors rs i len = 
    let (newRs,vect)=tosVector rs len in
    let (newRs2,recVects)=tosNVectors newRs (i-1) len in
    (newRs2,[vect] ++ recVects)

tosVector :: MiniStr -> Int -> (MiniStr,[Int]) 
tosVector rs 0 = (rs,[])
tosVector rs i = 
    let (newRs,int)=tosBoolInt rs in
    let (newRs2,recVect)=tosVector newRs (i-1) in
    (newRs2,[int] ++ recVect)

tosInt :: MiniStr -> (MiniStr,Int) 
tosInt rs = let (x:xs) =  intRands rs in
    (rs {intRands=xs},x)


tosBoolInt :: MiniStr -> (MiniStr,Int) 
tosBoolInt rs = let (x:xs) =  boolRands rs in
    (rs {boolRands=xs},x)

tosDouble :: MiniStr -> (MiniStr,Double) 
tosDouble rs = let (x:xs) =  doubleRands rs in
    (rs {doubleRands=xs},x)

tosChance ::  MiniStr ->Double-> (MiniStr,Bool) 
tosChance rs chance = let (x:xs) =  doubleRands rs in
    if x<chance then
        (rs {doubleRands=xs},True)
    else
        (rs {doubleRands=xs},False)
