{-# LANGUAGE OverloadedStrings #-}
module Crawler
    where 
        import Text.HTML.Scalpel ( texts, scrapeStringLike, (@:), hasClass )
        import Data.List ( isInfixOf )
        import Network.Curl ( curlGetString )
        import PrintPrice 
        
        type URL = String
        type ProductList = [(String,String)]
        type HTMLCode = String

        crawler :: String -> String -> IO ()
        crawler name serie = do  
          komplettHTML <- curlGetString komplettUrl []
          if serie == "3060" then do
                inetHTML <- curlGetString inet3060 []
                outputToFile (name ++ " " ++ serie) ("Komplett",(komplettCrawler name serie (snd komplettHTML))) ("Inet",(inetCrawler name (snd inetHTML)))
          else if serie == "3070" then do 
                inetHTML <- curlGetString inet3070 []
                outputToFile (name ++ " " ++ serie) ("Komplett",(komplettCrawler name serie (snd komplettHTML))) ("Inet",(inetCrawler name (snd inetHTML)))     
          else do 
                inetHTML <- curlGetString inet3080 []
                inetTIHTML <- curlGetString inet3080ti []
                outputToFile (name ++ " " ++ serie) ("Komplett",(komplettCrawler name serie (snd komplettHTML))) ("Inet",(inetCrawler name (snd inetHTML))++(inetCrawler name (snd inetTIHTML)))


        {- komplettCrawler name serie htmlCode
          takes in two search words and returns every intances of items that match the criteria in htmlCode
        RETURNS: a list with tuples of the name and price from the htmlCode that match both name and serie
        EXAMPLES: komplettCrawler "" "" "<!DOCTYPE html> <html> <span class='product-price-now'>2500</span> <h2>RTX 3080</h2> </html>" == [("RTX 3080","2500")]
                  komplettCrawler "foo" "bar" "<!DOCTYPE html> <html> <span class='product-price-now'>2500</span> <h2>RTX 3080</h2> </html>" == []
                  komplerrCrawler "" "" "" == []
        -}
        komplettCrawler :: String -> String -> HTMLCode -> ProductList
        komplettCrawler name serie htmlCode = komplettSearch (komplettPairNmPr htmlCode) name serie []
            

        {- komplettSearch pList name serie newList
          takes in a pList and returns every intance of items that meet the criteria set by name and serie
        RETURNS: a list with every item of pList that have name and serie in the string 
        EXAMPLES: komplettSearch [("foo","foo"),("bar","bar"),("foobar","baz")] "foo" "bar" [] == [("foobar","baz")]
                  komplettSearch [] "" "" [] == []
        VARIANT: length pList 
        -}    
        komplettSearch :: ProductList -> String -> String -> ProductList -> ProductList
        komplettSearch [] _ _ newList = reverse newList
        komplettSearch ((x,y):xs) name serie newList 
            |isInfixOf name x && isInfixOf serie x = komplettSearch xs name serie ((x,y):newList) 
            |otherwise = komplettSearch xs name serie newList


        {- komplettPairNmPr htmlCode
          takes in a htmlCode and returns all names and prices found in the htmlCode
        RETURNS: a list of tuples with names and prices found in the htmlCode
        EXAMPLES: komplettPairNmPr "<!DOCTYPE html> <html> <span class='product-price-now'>2500</span> <h2>RTX 3080</h2> </html>" == [("RTX 3080","2500")]
                  komplettPairNmPr "" == []
        -}
        komplettPairNmPr :: HTMLCode -> ProductList
        komplettPairNmPr htmlCode = priceAndNamePairer (komplettName htmlCode) (komplettPrice htmlCode) []


        {- kompletPrice htmlCode 
          finds all instances of texts in a span that has the class "product-price-now"
        RETURNS: a list with all instances found as strings
        EXAMPLES: komplettPrice "<!DOCTYPE html> <html> <span class='product-price-now'>2500</span> <h2>RTX 3080</h2> </html>" == ["2500"]
                  komplettPrice "" == []
        -}
        komplettPrice :: HTMLCode -> [String]
        komplettPrice htmlCode = case (scrapeStringLike htmlCode ( texts $ "span" @: [hasClass "product-price-now"])) of 
            Just a -> a
            Nothing -> []


        {- komplettName htmlCOde
          fids all instances of text inside "h2" 
        RETURNS: a list with a instances found as strings
        EXAMPLES: komplettName "<!DOCTYPE html> <html> <span class='product-price-now'>2500</span> <h2>RTX 3080</h2> </html>" == ["RTX 3080"]
                  komplettName "" == []
        -}
        komplettName :: HTMLCode -> [String]
        komplettName htmlCode = case (scrapeStringLike htmlCode (texts "h2")) of 
            Just a -> a
            Nothing -> []


        {- priceAndNamePairer names prices pLst
          merges the first item in names with the first item in prices as a tuple and continues
          until one list is empty
        RETURNS: pLst with the two lists merged together in tuples
        EXAMPLES: priceAndNamePairer ["foo","bar"] ["bar","foo"] [] == [("bar","foo"),("foo","bar")]
                  priceAndNamePairer ["foo","bar"] ["bar","foo"] [] == [("bar","foo"),("foo","bar")]
                  priceAndNamePairer [] [] [] == []
        VARIANT: length names and length prices
        -}
        priceAndNamePairer :: [String] -> [String] -> ProductList -> ProductList
        priceAndNamePairer _ [] lst = lst
        priceAndNamePairer [] _ lst = lst
        priceAndNamePairer (x:xs) (y:ys) lst = priceAndNamePairer xs ys ((x,y):lst)


        {- inetCrawler name htmlCode 
          finds all instances with name as a substring in htmlCode and returns them in a list 
        RETURNS: a list with all items that has name as a substring
        EXAMPLES: inetCrawler "RTX" "<!DOCTYPE html> <html> <span class='price'>2500</span> <h4>RTX 3080</h4> </html>" == [("RTX 3080","2500")]
                  inetCrawler "" "<!DOCTYPE html> <html> <span class='price'>2500</span> <h4>RTX 3080</h4> </html>" == [("RTX 3080","2500")]
                  inetCrawler "foo" "<!DOCTYPE html> <html> <span class='price'>2500</span> <h4>RTX 3080</h4> </html>" == []
                  inetCrawler "foo" "" == []
        -}      
        inetCrawler :: String -> HTMLCode -> ProductList
        inetCrawler name htmlCode = inetSearch (inetPair (inetName htmlCode) (inetPrice htmlCode)) name []


        {- inetSearch pList name newList
          finds all strings that have name as a substring in the first item and returns the instances
        RETURNS: a list with all the items in pList that have name as substrings
        EXAMPLES: inetSearch [("foo","bar"),("foobar","bar"),("bar","foo")] == [("foo","bar"),("foobar","bar")]
                  inetSearch [] "foo" [] == []
        VARIANT: length pList
        -}
        inetSearch ::ProductList -> String -> ProductList -> ProductList
        inetSearch [] _ newList = reverse newList
        inetSearch ((x,y):xs) name newList 
            |isInfixOf name x = inetSearch xs name ((x,y):newList) 
            |otherwise = inetSearch xs name newList

        
        {- inetPrice htmlCode
          finds and returns all instances of text in a span that has the class "price"
        RETURNS: list of all instances found in htmlCode
        EXAMPLES: inetPrice "<!DOCTYPE html> <html> <span class='price'>2500</span> <h4>RTX 3080</h4> </html>" == ["2500"]
                  inetprice "" == []
        -}
        inetPrice :: HTMLCode -> [String]
        inetPrice htmlCode = case (scrapeStringLike htmlCode ( texts $ "span" @: [hasClass "price"])) of 
                    Just a -> a
                    Nothing -> []
        

        {- inetName htmlCode 
          finds and returns every instance of text in "h4" in htmlCode
        RETURNS: list of all instances found in htmlCode
        EXAMPLES: inetName "<!DOCTYPE html> <html> <span class='price'>2500</span> <h4>RTX 3080</h4> </html>" == ["RTX 3080"]
                  inetName "" == [] 
        -}
        inetName :: HTMLCode -> [String]
        inetName htmlCode = case (scrapeStringLike htmlCode (texts $ "h4" )) of
                    Just a -> a
                    Nothing -> []


        {- inetPair [Names] [Prices]
          takes in two lists and pairs the firts items from both lists in a tuple 
        RETURNS: a list with pairs of the sent in lists
        EXAMPLES: inetPair ["foo","bar","baz"] ["baz","bar","foo"] == [("foo","baz"),("bar","bar"),("baz","foo")]
                  inetPair [] [] == []
        -}    
        inetPair :: [String] -> [String] -> ProductList
        inetPair names prices = priceAndNamePairer names prices []


        {-
        URLs corresponding the websites that needs to be serached by the crawler
        -}
        inet3060 :: URL
        inet3060 = "https://www.inet.se/kategori/164/grafikkort-gpu?filters=%7B%2229%22%3A%7B%22type%22%3A%22PropAny%22%2C%22any%22%3A%5B14700%2C14764%5D%7D%7D"
        inet3070 :: URL
        inet3070 = "https://www.inet.se/kategori/164/grafikkort-gpu?filters=%7B%2229%22%3A%7B%22type%22%3A%22PropAny%22%2C%22any%22%3A%5B14297%2C15755%5D%7D%7D"
        inet3080 :: URL
        inet3080 = "https://www.inet.se/kategori/164/grafikkort-gpu?filters=%7B%2229%22%3A%7B%22type%22%3A%22PropAny%22%2C%22any%22%3A%5B14294%5D%7D%7D"
        inet3080ti :: URL
        inet3080ti = "https://www.inet.se/kategori/164/grafikkort-gpu?filters=%7B%2229%22%3A%7B%22type%22%3A%22PropAny%22%2C%22any%22%3A%5B15734%5D%7D%7D"
        komplettUrl :: URL
        komplettUrl = "https://www.komplett.se/category/10412/datorutrustning/datorkomponenter/grafikkort?nlevel=10000%C2%A728003%C2%A710412&sort=PriceDesc%3ADESCENDING&hits=336"

        {-
        Some mock html codes used for test cases and for examples
        -}