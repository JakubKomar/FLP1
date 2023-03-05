{-
* file: Main.hs
* project: FLP 2022/2023 – funkcionální projekt: Haskell [Knapsack problem]
* autor: Jakub Komárek (xkomar33)
* description: links and run modules
-}

import System.Environment
import System.Exit

import ParseInput 
--import Types
{-
import BrutForce
import Minimize
import Types
-}
main :: IO ()
main = getArgs >>= parse

parse :: [[Char]] -> IO ()
parse ["-h"] =putStrLn "Použití: flp22-fun [-i -b -o] \"soubor\""  >> exitWith ExitSuccess

parse ["-i"] =putStrLn "Informace:" >> parseIputStdIn 
parse ["-i",fp] =putStrLn "Informace:" >> parseInputFile fp

parse ["-b"] = putStrLn "Prohledávání stavového prostoru hrubou silou:" >>parseIputStdIn 
parse ["-b",fp] =putStrLn "Prohledávání stavového prostoru hrubou silou:">>parseInputFile fp

parse ["-o"] =putStrLn "Genetic algorithm or other shit:" >>parseIputStdIn 
parse ["-o",fp] =putStrLn "Genetic algorithm or other shit:">> parseInputFile fp

parse _ =putStrLn "Args error, try -h" >>   exitWith  (ExitFailure 2)


