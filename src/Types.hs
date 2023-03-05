{-
* file: Types.hs
* project: FLP 2022/2023 – funkcionální projekt: Haskell [Knapsack problem]
* autor: Jakub Komárek (xkomar33)
* description: Definition of custume types
-}

module Types where
    
data KnapSack = Knapsack {
    maxWeight   :: Integer,
    minCost     :: Integer,
    items       :: [Item]
} deriving Show

data Item = Item{
    weight   :: Integer,
    cost     :: Integer
} deriving Show