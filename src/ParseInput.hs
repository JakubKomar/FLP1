{-
* file: ParseInput.hs
* project: FLP 2022/2023 – funkcionální projekt: Haskell [Knapsack problem]
* autor: Jakub Komárek (xkomar33)
* description: Parse args and files
-}

module ParseInput where
--import Types

parseIputStdIn :: IO ()
parseIputStdIn  = do 
    contents <- getContents
    parseText contents

parseInputFile :: FilePath -> IO ()
parseInputFile fs = do 
    file_content <- readFile fs
    parseText file_content


parseText :: String -> IO ()
parseText str = print str



{-
Knapsack {
    maxWeight =35,
    minCost =65,
    items =   [ 
        Item {weight=12 , cost=36} ,
        Item {weight=1 , cost=3} ,
        Item {weight=16 , cost=6} 
    ]
}
-}