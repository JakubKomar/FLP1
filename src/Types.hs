{-
* file: Types.hs
* project: FLP 2022/2023 – funkcionální projekt: Haskell [Knapsack problem]
* autor: Bc. Jakub Komárek (xkomar33)
* description: Definition of custume types
-}

module Types where
    
-- struktura problému KnapSac
data KnapSack = KnapSack {  
    maxWeight   :: Int,
    minCost     :: Int,
    items       :: [Item]
} deriving (Eq)

instance Show KnapSack where
    show c= "KnapSack {\n" 
            ++ "maxWeight: " ++ shows (maxWeight c) "\n"
            ++ "minCost: " ++ shows (minCost c) "\n"
            ++ "items: ["
            ++ itemsToString (items c)
            ++ "\n]\n}"

-- položka v batohu
data Item = Item{
    weight   :: Int,
    cost     :: Int  
} deriving (Eq)

itemsToString:: [Item]->String
itemsToString []=""
itemsToString (x:xs)=  (show x) ++ itemsToString xs
instance Show Item where
    show c=  "\n    Item {\n"
            ++ "    weight: " ++ shows (weight c) "\n"
            ++ "    cost: " ++ shows (cost c) "\n"
            ++ "    }"


-- pomocná struktura charkterizující řešení daného knapsack problému  
data SolutionVariation = SolutionVariation{
    itemVector :: [Int], -- vector nul a jedniček simbolizující, zdali je daný item vložen do knapsaku 
    weightSum :: Int, -- celková hmotnost
    costSum :: Int, -- celková suma
    valid :: Bool -- instance spnlňuje podmínky řešení knapsaku
} deriving (Eq,Show)

-- pomocná struktura optimalizační metody, obsahuje nekonečné seznamy náhodných čísel
data MiniStr=MiniStr {
    intRands :: [Int],  -- nekonečný seznam náhodných hodnot int
    boolRands :: [Int], -- nekonečný seznam náhodných hodnot int v rozsahu 0-1
    doubleRands :: [Double], -- nekonečný seznam náhodných hodnot double
    intCnt :: Int -- čítač iterací generického algoritmu
}  deriving (Eq)