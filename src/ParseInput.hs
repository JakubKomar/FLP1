{-
* file: ParseInput.hs
* project: FLP 2022/2023 – funkcionální projekt: Haskell [Knapsack problem]
* autor: Bc. Jakub Komárek (xkomar33)
* description: parsing vstupního řetězce a převod na knapsack konstrukt
-}
module ParseInput where

import Data.List.Split(splitOn)
import Data.Char(toLower)

import Types  -- uživatelsky definované typy

-- hlavní funkce na získání instance knapsak z textu
parseText :: String -> KnapSack 
parseText str = startParsing $ arasteEmpy$ splitByEOL $ dropWiteSpace $ map toLower str -- text se nejdříve převede na malá písmena, 
                                                                                        -- poté se zahodí všechny bílé znaky a text se rozdělí podle znaků konce řádků
                                                                                        -- prázdné prvky jsou zahozeny a následně je nad tímto polem proveden další parsing

-- zahazuje bílé znaky
dropWiteSpace :: String -> String
dropWiteSpace []=[]
dropWiteSpace (x:xs)= if  x /= '\r'  && x /= '\t' && x /= ' ' then
        x: dropWiteSpace xs
    else
        dropWiteSpace xs

-- maže prázdné prvky pole
arasteEmpy ::[String] -> [String]
arasteEmpy [] = []
arasteEmpy (x:xs) = if x== "" then arasteEmpy xs else x: arasteEmpy xs

-- rozdělí text podle znaku konce řádku
splitByEOL :: String -> [String]
splitByEOL str = splitOn "\n" str


-- začátek parsování, kontrola root elementu
startParsing ::[String] -> KnapSack
startParsing ("knapsack{":xs) = parametrParser xs 
startParsing ("knapsack":"{":xs) = parametrParser xs 
startParsing _ = parseErr

-- vytvoření prázdné instance knapsacku a kontrola řádného naplnění
parametrParser :: [String] -> KnapSack
parametrParser x = let kp=KnapSack {maxWeight= -1 , minCost= -1 , items= []}in 
    let parsedKp=subParametrParser  x kp in
        if knapsackCheckCorrectnes parsedKp then
            parsedKp
        else 
            parseErr

-- kontroluje korektnost instance KnapSack
knapsackCheckCorrectnes :: KnapSack ->Bool
knapsackCheckCorrectnes kp = maxWeight kp >0 && minCost kp >0 && items kp /=[]

-- parsing obsahu rootovského elementu
subParametrParser :: [String] -> KnapSack -> KnapSack
subParametrParser [] kp = kp
subParametrParser (x:xs) kp =  let sl= splitOn ":" x  in 
        if (length sl)==1 && sl!!0=="}" then
            kp
        else if (length sl) /= 2 then
            parseErr
        else if sl!!0 == "maxweight" then
            subParametrParser xs kp {maxWeight= read  $ sl!!1 :: Int}
        else if sl!!0 == "mincost" then 
            subParametrParser xs kp {minCost= read  $ sl!!1 :: Int}
        else if sl!!0 == "items" && sl!!1=="[" then 
            let (itmS,rest)=itemsParser xs in   
            subParametrParser rest kp {items= itmS} 
        else
            parseErr

-- parser položek knapsaku
itemsParser :: [String] -> ([Item],[String])
itemsParser ("]":xs)= ([],xs)
itemsParser ("item{":xs) = let (itm,rest)= itemParser2 xs in
    let (recItems,recRest) = itemsParser(rest) in
    ([itm] ++ recItems,recRest)
itemsParser ("item":"{":xs)=  let (itm,rest)= itemParser2 xs in
    let (recItems,recRest) = itemsParser(rest) in
    ([itm] ++ recItems,recRest)
itemsParser _ = parseErr

-- tvorba prázdné instance itemu a kontrola zprávného napnění
itemParser2 :: [String]  -> (Item,[String] )
itemParser2 xs = let it=Item {weight= -1 , cost= -1 }in 
    let (itm, rest)= itemParser3 xs it in
        if itemCheckCorrectnes itm then
            (itm, rest)
        else
            parseErr

-- kontroluje korektnost instance item
itemCheckCorrectnes :: Item ->Bool
itemCheckCorrectnes it = weight it >0 && cost it >0 

-- parsing jednotlivých elementů každého itemu
itemParser3 :: [String] -> Item  ->  (Item,[String] )
itemParser3 (x:xs) it =  let sl= splitOn ":" x  in
        if length sl==1 && sl!!0=="}" then
            (it,xs) 
        else if length sl/=2 then
            parseErr
        else if sl!!0 == "weight" then
            itemParser3 xs it {weight= read  $ sl!!1 :: Int}
        else if sl!!0 == "cost" then 
            itemParser3 xs it {cost= read  $ sl!!1 :: Int}
        else 
            parseErr
itemParser3 _ _ =parseErr

-- chybová hláška při prasingu
parseErr ::  a 
parseErr = error "Chyba ve vstupu"