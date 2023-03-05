{-
* file: Main.hs
* project: FLP 2022/2023 – funkcionální projekt: Haskell [Knapsack problem]
* autor: Jakub Komárek (xkomar33)
* description: links and run modules
-}

import System.Environment
import System.Exit

import ParseInput 
import BrutForce
import Minimize
import Types

main :: IO ()
main = getArgs>>= parse

parse ["-h"] =putStrLn "Použití: flp22-fun [-i -b -o] \"soubor\""  >> exitWith ExitSuccess

parse ["-i"] =parseInput 
parse ["-i",fp] =parseInputFs fp

parse ["-b"] =putStrLn "Prohledávání stavového prostoru hrubou silou"
parse ["-b",fp] =putStrLn "Prohledávání stavového prostoru hrubou silou"

parse ["-o"] =putStrLn "Genetic algorithm or other shit" 
parse ["-o",fp] =putStrLn "Genetic algorithm or other shit" 

parse _ =putStrLn "Args err, try -h" >>   exitWith  (ExitFailure 2)

printIns= putStrLn "Vypsat instanci"  

--parse fs     = concat `fmap` mapM readFile fs
