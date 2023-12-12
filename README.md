# FLP 2022/2023 – funkcionální projekt: Haskell [Knapsack problem]
#### autor: Bc. Jakub Komárek (xkomar33)
#### description: program řeší optimalizační verzi 0-1 problému batohu (knapsack problem)

použití: ./flp22-fun [-i -b -o] ["soubor.txt" <"soubor.txt"]

argumenty: -i : ze vstupu načte informace o instanci knapsack do vaší vnitřní reprezentace. Na stdout jí vypíše zpět -b : ze vstupu načte informace o knapsack instanci. Na stdout vypíše řešení nalezené prohledáváním stavového prostoru hrubou silou. V případě že řešení nebylo nalezeno, vypíše False. -o : ze vstupu načte informace o knapsack instanci. Na stdout vypíše řešení nalezené pomocí optimalizační metody genetic algorithm. V případě že řešení nebylo nalezeno, vypíše False.

příklady použití: ./flp22-fun -i "test.txt" ./flp22-fun -b "test.txt" ./flp22-fun -o < "test.txt"
