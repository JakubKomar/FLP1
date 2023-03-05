{-
* file: ParseInput.hs
* project: FLP 2022/2023 – funkcionální projekt: Haskell [Knapsack problem]
* autor: Jakub Komárek (xkomar33)
* description: Parse args and files
-}

module ParseInput where
import Data.List.Split
import Data.Char
import Types
import Debug.Trace
parseIputStdIn :: IO ()
parseIputStdIn  = do 
    contents <- getContents
    parseText contents

parseInputFile :: FilePath -> IO ()
parseInputFile fs = do 
    file_content <- readFile fs
    parseText file_content


parseText :: String -> IO ()
parseText str = print $ startParsing $ arasteEmpy$ splitByEOL $ dropWiteSpace $ map toLower str

dropWiteSpace :: String -> String
dropWiteSpace []=[]
dropWiteSpace (x:xs)= if  x /= '\r'  && x /= '\t' && x /= ' ' then
        x: dropWiteSpace xs
    else
        dropWiteSpace xs


arasteEmpy ::[String] -> [String]
arasteEmpy [] = []
arasteEmpy (x:xs) = if x== "" then arasteEmpy xs else x: arasteEmpy xs


splitByEOL :: String -> [String]
splitByEOL str = splitOn "\n" str

startParsing ::[String] -> KnapSack
startParsing ("knapsack{":xs) = parametrParser xs 
startParsing ("knapsack":"{":xs) = parametrParser xs 
startParsing _ = parseErr

parametrParser :: [String] -> KnapSack
parametrParser x = let kp=KnapSack {maxWeight= -1 , minCost= -1 , items= []}in 
    subParametrParser  x kp
parametrParser _ = parseErr

subParametrParser :: [String] -> KnapSack -> KnapSack
subParametrParser [] kp = kp
subParametrParser (x:xs) kp =  let sl= splitOn ":" x 
    in traceShow sl$
        if length sl==1 && sl!!1=="}" then
            kp
        else if length sl /= 2 then
            parseErr
        else if sl!!0 == "maxweight" then
            subParametrParser xs kp {maxWeight= read  $ sl!!1 :: Int}
        else if sl!!0 == "mincost" then 
            subParametrParser xs kp {minCost= read  $ sl!!1 :: Int}
        else if sl!!0 == "items" && sl!!1=="[" then 
           subParametrParser xs kp {items= itemsParser xs} 

subParametrParser _ _ =parseErr

itemsParser :: [String]  -> [item]
itemsParser ("]":xs)= []
itemsParser ("item{":xs) =  itemParser2 xs
itemsParser ("item":"{":xs)= itemParser2 xs  
itemsParser _ = parseErr

itemParser2 :: [String]  -> Item
itemParser2 xs = let it=Item {weight= -1 , cost= -1 }in 
    itemParser3 xs it
itemParser2 _ = parseErr

itemParser3 :: [String] -> Item  -> Item 
itemParser3 [] it = it
itemParser3 (x:xs) it =  let sl= splitOn ":" x  in
        traceShow sl$
        if length sl==1 && sl!!0=="}" then
            it
        else if length sl/=2 then
            parseErr
        else if sl!!0 == "weight" then
            itemParser3 xs it {weight= read  $ sl!!1 :: Int}
        else if sl!!0 == "cost" then 
            itemParser3 xs it {cost= read  $ sl!!1 :: Int}
        else 
            parseErr
itemParser3 _ _ =parseErr



parseErr = error "Chyba ve vstupu"
parseErr2 = error "wadaw"
{-

Knapsack {
    maxWeight =35,
    minCost =65,
    items =   [ 
        Item {weight=12 , cost=36} ,
        Item {weight=1 , cost=3} ,
        Item {weight=16 , cost=6} 
    ]
}
-}