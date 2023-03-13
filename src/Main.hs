{-
* file: Main.hs
* project: FLP 2022/2023 – funkcionální projekt: Haskell [Knapsack problem]
* autor: Bc. Jakub Komárek (xkomar33)
* description: parsuje argumenty a zpouští hlavní moduly
-}
import System.Environment(getArgs)
import System.Exit(exitWith, ExitCode( ExitFailure,ExitSuccess ))
import Minimize(minimaze)
import System.Random(getStdGen)

import ParseInput(parseText)
import BrutForce(brutforce)
import Data.Maybe(fromJust)
import Types(SolutionVariation(itemVector)) -- uživatelsky definované typy

-- hlavní funkce
main :: IO ()
main = getArgs >>= parse

-- tato funkce zkontroluje argumenty a získá data ze souboru/stdin. Pokračuje ve funkci parse2
parse :: [[Char]] -> IO ()
parse ["-h"] =printHelp
parse [x,fp ]= -- vstup ze souboru
     if checkOption(x) then do
        file_content <- readFile fp
        parse2 x file_content
    else
        argsErr
parse [x]= -- vstup ze stdin
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
    printResult sol
parse2 "-o" context = do
    gen <- getStdGen -- vytvoření nového seedu pro generování náhodných čísel
    let sol = minimaze (parseText  context)  gen
    printResult sol
parse2 _ _ = argsErr

-- formátovací funkce pro výpis výsledku
printResult :: Maybe SolutionVariation -> IO()
printResult sol =  
    if sol==Nothing then
        print False
    else
        putStrLn ( "Solution " ++ formatVector(itemVector $ fromJust sol))

-- převede vektor na string
formatVector :: [Int] -> String
formatVector x = "[" ++ formatVector2 x ++ "]"

-- převede vektor na string
formatVector2 :: [Int] -> String
formatVector2 []= ""
formatVector2 (x:[]) = (show x) 
formatVector2 (x:xs) = (show x) ++ " " ++ (formatVector2 xs)

-- chyba při kontrole argumentů
argsErr :: IO ()
argsErr=putStrLn "Args error, try -h" >>   exitWith  (ExitFailure 2)

printHelp ::IO ()
printHelp = putStrLn "\
\Autor: Bc. Jakub Komárek (xkomar33)\n\ 
\Project: FLP 2022/2023 – funkcionální projekt: Haskell [Knapsack problem]\n\ 
\Description: program řeší optimalizační verzi 0-1 problému batohu (knapsack problem)\n\ 
\\n\ 
\Použití:\n\  
\   ./flp22-fun [-i -b -o] [\"soubor.txt\" <\"soubor.txt\"] \n\ 
\Argumenty: \n\ 
\    -i :ze vstupu načte informace o instanci knapsack do vaší vnitřní reprezentace. Na stdout jí vypíše zpět \n\ 
\    -b :ze vstupu načte informace o knapsack instanci. Na stdout vypíše řešení nalezené prohledáváním stavového prostoru hrubou silou.\n\
\        V případě že řešení nebylo nalezeno, vypíše False. \n\ 
\    -o :ze vstupu načte informace o knapsack instanci. Na stdout vypíše řešení nalezené pomocí optimalizační metody genetic algorithm.\n\ 
\        V případě že řešení nebylo nalezeno, vypíše False." >> exitWith ExitSuccess