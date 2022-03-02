


webbhallen = ("Webbhallen",[("rtx 3060",2300),("rtx3070", 4200),("rtx3080", 12000)])

komplett = ("Komplett",[("rtx 3060",2400),("rtx3070", 4500),("rtx3080", 11000)])

netonnet = ("Net on Net",[("rtx 3050", 1000),("rtx 3060",2700),("rtx3070", 3300),("rtx3080", 14000)])




mergeTuple tuple1 tuple2 tuple3 = 
    [((fst tuple1),(fst tuple2),(fst tuple3))]



tupleToList tuple1 tuple2 tuple3 = merge (snd tuple1) (snd tuple2) (snd tuple3)

merge (x:xs) (y:ys) (z:zs) = 
    mergeAUX x (y:ys) (z:zs)
    
-- mergeAUX (a,b) [] = []
mergeAUX (a,b) (y:ys) (z:zs) =
    sndMerge (fstMerge (a,b) (y:ys)) (z:zs)

fstMerge :: (String,Integer) -> [(String,Integer)] -> (String,Integer,Integer)
fstMerge (a,b) [] = (a,b,0)
fstMerge (a,b) (y:ys) = 
    if a == fst y then (a,b,snd y)
    else fstMerge (a,b) ys

sndMerge (a,b,c) [] = [(a,b,c,0)]
sndMerge (a,b,c) (z:zs) = 
    if a == fst z then [(a,b,c,snd z)]
    else sndMerge (a,b,c) zs





createFile ((a,b,c):xs) = do
                        writeFile "RTX card name.csv" ("model;" ++ a ++ ";" ++ b ++ ";" ++ c)
    

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
{-
printShop tupl1 tupl2 tupl3 = do
    webside1 <- map putStr (fst tupl1)
    putStrLn (("Graffikkort-namn     pris på ") ++ webside1)
    -}