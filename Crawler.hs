{-# LANGUAGE OverloadedStrings #-}
import Text.HTML.Scalpel
import Komplett
import Data.List ( isInfixOf )
import Network.Curl


komplett :: String -> String -> IO [(String, String)]
komplett brand serie = do
    komplettStr <- curlGetString komplettUrl method_GET
    return (komplettCrawler brand serie (snd komplettStr))


komplettUrl :: String
komplettUrl = "https://www.komplett.se/category/10412/datorutrustning/datorkomponenter/grafikkort?nlevel=10000%C2%A728003%C2%A710412&sort=PriceDesc%3ADESCENDING&hits=336"


komplettCrawler :: String -> String -> String -> [(String, String)]
komplettCrawler brand serie urlString = komplettBrand (komplettSerie (komplettPairNmPr urlString) serie []) brand []
    

komplettSerie [] _ fixLst = fixLst
komplettSerie ((x,y):xs) nm fixLst 
    |isInfixOf nm x = komplettSerie xs nm ((x,y):fixLst)
    |otherwise = komplettSerie xs nm fixLst

komplettBrand [] _ fixLst = fixLst
komplettBrand ((x,y):xs) brand fixLst
    |isInfixOf brand x = komplettBrand xs brand ((x,y):fixLst)
    |otherwise = komplettBrand xs brand fixLst   

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

