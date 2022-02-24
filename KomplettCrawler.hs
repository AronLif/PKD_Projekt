{-# LANGUAGE OverloadedStrings #-}
module KomplettCrawler
    where 
        import Text.HTML.Scalpel ( texts, scrapeStringLike, (@:), hasClass )
        import Data.List ( isInfixOf )
        import Network.Curl ( curlGetString, method_GET )

        komplett :: String -> String -> IO ()
        komplett brand serie = do
            komplettStr <- curlGetString komplettUrl method_GET
            putStrLn "Komplett"
            prettyPrint (komplettCrawler brand serie (snd komplettStr)) []
           
        
        prettyPrint :: [(String, String)] -> [Char] -> IO ()
        prettyPrint [] str = putStr str 
        prettyPrint ((x,y):xs) str = prettyPrint xs (str ++ (x ++ " " ++ y ++ "\n")) 

        komplettUrl :: String
        komplettUrl = "https://www.komplett.se/category/10412/datorutrustning/datorkomponenter/grafikkort?nlevel=10000%C2%A728003%C2%A710412&sort=PriceDesc%3ADESCENDING&hits=336"

        komplettCrawler :: String -> String -> String -> [(String, String)]
        komplettCrawler brand serie urlString = komplettSearch (komplettPairNmPr urlString) brand serie []
            
        komplettSearch [] _ _ newList = reverse newList
        komplettSearch ((x,y):xs) name serie newList 
            |isInfixOf name x && isInfixOf serie x = komplettSearch xs name serie ((x,y):newList) 
            |otherwise = komplettSearch xs name serie newList


        komplettPairNmPr :: [Char] -> [([Char], [Char])]
        komplettPairNmPr str = komplettPairNmPrAux (komplettName str) (komplettPrice str) []

        komplettPrice :: String -> [String]
        komplettPrice str = case (scrapeStringLike str ( texts $ "span" @: [hasClass "product-price-now"])) of 
            Just a -> a
            Nothing -> []


        komplettName :: String -> [String]
        komplettName str = case (scrapeStringLike str (texts "h2")) of 
            Just a -> a
            Nothing -> []

        komplettPairNmPrAux :: [String] -> [String] -> [(String,String)] -> [(String,String)]
        komplettPairNmPrAux _ [] lst = lst
        komplettPairNmPrAux [] _ lst = lst
        komplettPairNmPrAux (x:xs) (y:ys) lst = komplettPairNmPrAux xs ys ((x,split "\160\&" y []):lst)

        split :: String -> String -> String -> String
        split rem [] newStr = newStr 
        split [] str newStr = newStr ++ str
        split rem str newStr 
            |head rem == head str = split (tail rem) (tail str) newStr
            |otherwise = split rem (tail str) (newStr ++ [head str])

