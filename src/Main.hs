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
main = getArgs>>= parse

parse :: [[Char]] -> IO ()
parse ["-h"] =putStrLn "Použití: flp22-fun [-i -b -o] \"soubor\""  >> exitWith ExitSuccess

parse ["-i"] =parseInput 
parse ["-i",fp] =parseInputFs fp

parse ["-b"] = putStrLn "Prohledávání stavového prostoru hrubou silou" >>parseInput 
parse ["-b",fp] =putStrLn "Prohledávání stavového prostoru hrubou silou">>parseInputFs fp

parse ["-o"] =putStrLn "Genetic algorithm or other shit" >>parseInput 
parse ["-o",fp] =putStrLn "Genetic algorithm or other shit">> parseInputFs fp

parse _ =putStrLn "Args err, try -h" >>   exitWith  (ExitFailure 2)


