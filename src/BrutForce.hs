{-
* file: BrutForce.hs
* project: FLP 2022/2023 – funkcionální projekt: Haskell [Knapsack problem]
* autor: Jakub Komárek (xkomar33)
* description: brut force solving method
-}

module BrutForce where
import Types

brutforce :: KnapSack -> [Int]
brutforce ks = [0]