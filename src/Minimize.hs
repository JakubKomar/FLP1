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

-- minimaze :: KnapSack -> IO ()
minimaze ks = do
    gen <- getStdGen 
    let rs = MiniStr {boolRands= (randomRs (0, 1 :: Int) gen ), intRands=randoms  gen,doubleRands= (randomRs(0, 1 :: Double) gen ), intCnt=  0} 
    let (x)= startMinimaze ks rs
    return $ x


-- startMinimaze :: KnapSack -> MiniStr -> Int
startMinimaze ks rs= 
    let (sols, newRs)= genereteSolutions 16 ks rs in
    begginGenAlg newRs  ks sols

-- begginGenAlg :: MiniStr -> KnapSack ->[SolutionVariation] -> 
begginGenAlg rs ks sols= sols

{-}
shuffle  :: MiniStr ->[a] -> (MiniStr,[a] )
shuffle rs []= (rs,[])
shuffle rs (x:xy)=
    let (newRs,int)=abs(tosInt (rs))in
    let pos =  int `mod` ((length xy )+1 ) in
    if pos == 0 then
        let (newRs2,rest)=shuffle newRs xy
        (newRs2,[x] ++ rest)
    else 
        letr
        (newRs2,[ xy !! (pos-1) ] ++ rest)
       
-}
abs :: Int ->Int
abs x = if x>=0 then
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

tosChance ::  MiniStr -> (MiniStr,Bool) 
tosChance rs = let (x:xs) =  doubleRands rs in
    if x<0.10 then
        (rs {doubleRands=xs},True)
    else
        (rs {doubleRands=xs},False)
