{-
* file: Main.hs
* project: FLP 2022/2023 – funkcionální projekt: Haskell [Knapsack problem]
* autor: Jakub Komárek (xkomar33)
* description: parsuje argumenty a zpouští hlavní moduly
-}

import System.Environment
import System.Exit

import ParseInput 
import BrutForce
import Data.Maybe
import Types
import Minimize
import System.Random

main :: IO ()
main = getArgs >>= parse

-- tato funkce zkontroluje argumenty a získá data ze souboru/stdin. Pokračuje ve funkci parse2
parse :: [[Char]] -> IO ()
parse ["-h"] =putStrLn "Použití: flp22-fun [-i -b -o] \"soubor\""  >> exitWith ExitSuccess
-- vstup ze souboru
parse [x,fp ]=
    if checkOption(x) then do
        file_content <- readFile fp
        parse2 x file_content
    else
        argsErr
-- vstup ze stdin
parse [x]= 
    if checkOption(x) then do
        contents <- getContents
        parse2 x contents
    else
        argsErr
parse _=argsErr

-- kontrola validních módů zpuštění
checkOption:: [Char]->Bool
checkOption arg = arg=="-i"||arg=="-b"||arg=="-o"

-- hlavní větvení podle zpuštěného módu
parse2 :: [Char] -> String -> IO ()
parse2 "-i" context = print$ parseText context
parse2 "-b" context= 
    let sol =  brutforce $parseText  context in
    if sol==Nothing then
        print False
    else
        putStrLn ( vectorToShitFormat(itemVector $ fromJust sol))
parse2 "-o" context =do  
    res<-minimaze $ parseText  context 
    print res
parse2 _ _ = argsErr


vectorToShitFormat :: [Int] -> String
vectorToShitFormat x = "[ " ++ vectorToShitFormat2 x ++ "]"

vectorToShitFormat2 :: [Int] -> String
vectorToShitFormat2 []= ""
vectorToShitFormat2 (x:xs) = (show x) ++ " " ++ (vectorToShitFormat2 xs)

-- chyba při kontrole argumentů
argsErr :: IO ()
argsErr=putStrLn "Args error, try -h" >>   exitWith  (ExitFailure 2)