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
    in
        if length sl<2 then
            parseErr
        else if sl!!0 == "maxweight" then
            subParametrParser xs kp {maxWeight= read  $ sl!!1 :: Int}
        else if sl!!0 == "mincost" then 
            subParametrParser xs kp {minCost= read  $ sl!!1 :: Int}
        else if sl!!0 == "items" then
            itemsParser xs kp
        else 
            parseErr
subParametrParser _ _ =parseErr

itemsParser :: [String] -> KnapSack -> KnapSack



paramSeter :: [String] -> KnapSack-> KnapSack
--paramSeter 
paramSeter _ _= parseErr




parseErr = error "Chyba ve vstupu"
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