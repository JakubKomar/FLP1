{-
* file: Main.hs
* project: FLP 2022/2023 – funkcionální projekt: Haskell [Knapsack problem]
* autor: Jakub Komárek (xkomar33)
* description: parsuje argumenty a zpouští hlavní moduly
-}

{-

import Minimize
import Types -}

import System.Environment
import System.Exit

import ParseInput 
import BrutForce

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
parse2 "-b" context= print$ brutforce $parseText  context 
parse2 "-o" context =putStrLn "Genetic algorithm or other shit:" >>parse2 "-i" context 
parse2 _  _=argsErr

-- chyba při kontrole argumentů
argsErr :: IO ()
argsErr=putStrLn "Args error, try -h" >>   exitWith  (ExitFailure 2)