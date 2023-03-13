{-
* file: Minimize.hs
* project: FLP 2022/2023 – funkcionální projekt: Haskell [Knapsack problem]
* autor: Jakub Komárek (xkomar33)
* description: minimaze method
-}
module Minimize where

import System.Random(randomRs,randoms,StdGen)

import Types
import BrutForce(solutionsEvaluation,findSolution,solutionTransformation)

-- inicializační funkce - vytvoří seznamy nekonečných náhodných čísel a započne generický algoritmus
minimaze :: KnapSack -> StdGen -> Maybe SolutionVariation
minimaze ks gen = 
    let rs = MiniStr {boolRands= (randomRs (0, 1 :: Int) gen ), intRands=randoms  gen,doubleRands= (randomRs(0, 1 :: Double) gen ), intCnt=  0} in
    startMinimaze ks rs 

-- vytvoření první generace řešení - náhodné řešení
startMinimaze :: KnapSack -> MiniStr -> Maybe SolutionVariation
startMinimaze ks rs= 
    let (sols, newRs)= genereteSolutions 32 ks rs in
    begginGenAlg  newRs  ks sols

-- hlavní rekurzní cyklus generyckého algoritmu
begginGenAlg :: MiniStr -> KnapSack ->[SolutionVariation] -> Maybe SolutionVariation
begginGenAlg rs ks sols= 
    let (newRs,sufleSols)=shuffle rs sols in    -- zamáchání všech řešení
    let x=solutionsEvaluation sufleSols ks in   -- vyhodnocení řešení
    let y=findSolution x in -- pokus o nalezení řešení v aktuálním řešení
    if y/=Nothing then
        y   -- pokud bylo nalezeno, vracíme řešení
    else -- jinak pokračujeme vytvořením nové generace řešení a rekurzním voláním této funkce
        let (newRs1, fightedSol) = fightSolutions newRs ks x in -- porovnání dvojic řešení, zůstane pouze nejlepší, 
                                                                -- pokud jsou obě řešení nulová, vygeneruje se nové
        let (newRs2, fightedSufleSol) = shuffle newRs1 fightedSol  in -- zamýchání výslednách řešení
        let (newRs3, randInt) = tosInt newRs2 in  -- náhodné číslo, které bude použito jako dělící pilíř při tvorbě potomků
        let (newRs4, newGeneration) = createChilds newRs3 randInt fightedSufleSol in -- tvorba potomků
        let modNewRs=newRs4{intCnt=(intCnt newRs4)+1}in  -- inkrementace čítače iterací
        -- (trace $ show x)
        if (intCnt newRs4)<= 20000 then -- pokud bylo dosaženo maximálního počtu iterací je problém prohlášen za nevyřešitelný
            begginGenAlg modNewRs ks newGeneration  -- rekurzivní volání s nově vytvořeno generací řešení
        else
            Nothing 

-- mutace vektoru - na základě náhody jsou ve vektoru překlopeny některé nebo žádné bity z důvodu zabránění konvergence
mutateVector :: MiniStr -> [Int] -> (MiniStr, [Int])
mutateVector rs []= (rs,[])
mutateVector rs (x:xs) =
    let (newGen,randFactor) = tosChance rs 0.05 in -- bit se překlopí z 5% šancí
    let (newGen2,vectRest)= mutateVector newGen xs in -- rekurzní volání na zbytek vektoru
    if randFactor then
        if x==0 then
            (newGen2,[1] ++ vectRest )
        else
            (newGen2,[0] ++ vectRest )
    else
        (newGen2,[x] ++ vectRest )

-- vytvoření potomků ze dvou řešení, pilíř je pro každou generaci vygenerovan náhodně
createChilds :: MiniStr -> Int -> [SolutionVariation] -> (MiniStr,[SolutionVariation]) 
createChilds rs _ []=(rs,[])
createChilds rs _ [_]=(rs,[])
createChilds rs rand (x:y:xs) =
    let splitPos = normalizeRndInt rand  (length (itemVector x)) in -- výpočet dělící pozice na základě náhodného čísla
    let (ax,ay)=splAt splitPos (itemVector x) in    -- tvorba částí vektoru z prvního rodiče
    let (bx,by)=splAt splitPos (itemVector y) in    -- tvorba částí vektoru z druhého rodiče
    let (newGen,mutatedA) = mutateVector rs (ax++by) in -- tvorba prvního potomka a jeho mutace
    let (newGen2,mutatedB) = mutateVector newGen (bx++ay) in-- tvorba druhého potomka a jeho mutace
    let (newGen3,rest) = createChilds newGen2 rand xs in -- rekurzní volání pro zbytek řešení v dané generaci
    (newGen3,[x,y,SolutionVariation {itemVector= mutatedA,valid=False,weightSum=(-1),costSum=(-1)},SolutionVariation {itemVector= mutatedB,valid=False,weightSum=(-1),costSum=(-1)}] ++ rest)

-- funkce rozdělí vektor v dané pozici na dva
splAt :: Int -> [a]->([a],[a])
splAt _ []=([],[])
splAt 0 x = ([],x)
splAt i (x:xs) = 
    let (a,b)= splAt (i-1) xs in
    ([x]++a, b)

-- normalizuje (náhodné) číslo do rozsahu 0-n 
normalizeRndInt :: Int ->Int ->Int 
normalizeRndInt x upperBount= (absVal x) `mod` upperBount

--porovnání dvojic řešení, zůstane pouze nejlepší, pokud jsou obě řešení nulová, vygeneruje se nové
fightSolutions ::  MiniStr ->KnapSack-> [SolutionVariation] -> ( MiniStr ,[SolutionVariation] )
fightSolutions rs _ []= (rs,[])
fightSolutions rs _ [_]= (rs,[])
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

-- náhodné zamíchání listu
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

-- vymaže n-tý prvek z pole
removeN :: Int -> [a] ->[a]
removeN _ []=[]
removeN i (x:xs)=
    if i ==0 then
        removeN (i-1) xs 
    else
        [x]++ (removeN (i-1) xs )

-- absolutní hodnota
absVal :: Int ->Int
absVal x = if x>=0 then
            x
        else
            (-x) 

-- vygenerování náhodných n řešení - počátek generického algoritmu
genereteSolutions:: Int ->KnapSack -> MiniStr -> ([SolutionVariation],MiniStr)
genereteSolutions i ks rs= 
    let (newRs,vectors) = tosNVectors rs i $ length (items ks) in
    let sols= solutionTransformation vectors in
    (sols,newRs)

-- vytáhne n náhodných vektorů
tosNVectors :: MiniStr -> Int -> Int -> (MiniStr,[[Int]]) 
tosNVectors rs 0 _= (rs,[])
tosNVectors rs i len = 
    let (newRs,vect)=tosVector rs len in
    let (newRs2,recVects)=tosNVectors newRs (i-1) len in
    (newRs2,[vect] ++ recVects)

-- vytáhne náhodný vektor
tosVector :: MiniStr -> Int -> (MiniStr,[Int]) 
tosVector rs 0 = (rs,[])
tosVector rs i = 
    let (newRs,int)=tosBoolInt rs in
    let (newRs2,recVect)=tosVector newRs (i-1) in
    (newRs2,[int] ++ recVect)

-- vytáhne náhodné celé číslo
tosInt :: MiniStr -> (MiniStr,Int) 
tosInt rs = let (x:xs) =  intRands rs in
    (rs {intRands=xs},x)

-- vytáhne náhodné celé číslo v rozsahu 0-1
tosBoolInt :: MiniStr -> (MiniStr,Int) 
tosBoolInt rs = let (x:xs) =  boolRands rs in
    (rs {boolRands=xs},x)

-- vytáhne náhodné reálné číslo v rozsahu 0-1
tosDouble :: MiniStr -> (MiniStr,Double) 
tosDouble rs = let (x:xs) =  doubleRands rs in
    (rs {doubleRands=xs},x)

-- vytáhné náhodnou boolovskou hodnotu na základě zadané procentuelní šance
tosChance ::  MiniStr ->Double-> (MiniStr,Bool) 
tosChance rs chance = let (x:xs) =  doubleRands rs in
    if x<chance then
        (rs {doubleRands=xs},True)
    else
        (rs {doubleRands=xs},False)
