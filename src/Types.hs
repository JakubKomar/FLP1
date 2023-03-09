{-
* file: Types.hs
* project: FLP 2022/2023 – funkcionální projekt: Haskell [Knapsack problem]
* autor: Jakub Komárek (xkomar33)
* description: Definition of custume types
-}

module Types where
    
data KnapSack = KnapSack {
    maxWeight   :: Int  ,
    minCost     :: Int  ,
    items       :: [Item]
} deriving (Eq,Show)

data Item = Item{
    weight   :: Int  ,
    cost     :: Int  
} deriving (Eq,Show)

data SolutionVariation = SolutionVariation{
    itemVector :: [Int],
    weightSum :: Int,
    costSum :: Int,
    valid :: Bool
} deriving (Eq,Show)