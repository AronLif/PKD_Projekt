


webhallen = ("webbhallen",[("rtx 3060",2300),("rtx3070", 4200),("rtx3080", 12000)])

complett = ("complett",[("rtx 3060",2400),("rtx3070", 4500),("rtx3080", 11000)])

netonnet = ("Net on Net",[("rtx 3050", 1000),("rtx 3060",2700),("rtx3070", 3300),("rtx3080", 14000)])

-- test

-- TODO:
-- börja med att skapa en CSV fil
-- Printa första raden i CSV filen: Model,butik1,butik2,butik3
--
--
--
--
--
--

{-
storeName :: (String,b) -> String 
storeName tuple = fst tuple


strToPrint :: [String] -> [IO ()]
strToPrint store = 
    map putStrLn store


printShopAux tupl1 tupl2 tupl3 = 
    printShop 

printShop tupl1 tupl2 tupl3 = do
    webside1 <- strToPrint (storeName webhallen)
    webside2 <- strToPrint (storeName complett)
    webside3 <- strToPrint (storeName netonnet)
    putStrLn (("Graffikkort-namn     pris på ") ++ webside1 ++ " pris på " ++ webside2 ++ " pris på " ++ webside3 )
-}

printShop tupl1 tupl2 tupl3 = do
    webside1 <- map putStr (fst tupl1)
    putStrLn (("Graffikkort-namn     pris på ") ++ webside1)