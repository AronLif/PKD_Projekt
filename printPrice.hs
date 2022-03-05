module PrintPrice
    where

   
        {-SingleProductInfo
        this represents the data name and prise of a single product
        -}
        type SingleProductInfo = (String, String)

        {- MergedProductInfo
        This represents the combined data on each prduct taken from both websites        
        -}
        type MergedProductInfo = [(String,String,String)]
        
        {- CsvInfo
        This represents the String printed to the CSV with all the info in the produkts
        and all te speciall characthers for structuring the look in the CSV file
        -}
        type CsvInfo = String
        
        {- ProductInfo
        This represents the list that contains all tuples of product info
        -}
        type ProductsInfo = [SingleProductInfo]

        {- WebsiteName
        Represents the string in WebInfo that is the name of the website
        This is just to make things clearer
        -}
        type WebsiteName = String     
        
        {-WebInfo
        This represents the tuple that has all the info from the webcrawler
        -}
        type WebInfo = (WebsiteName,ProductsInfo)

        firstTitle = "modell"

        
        createCsvText :: MergedProductInfo -> CsvInfo
        createCsvText [] = []
        createCsvText ((a,b,c):xs) = 
            a ++ ";" ++ b ++ ";" ++ c ++ "\n" ++ createCsvText xs

               
        mergeProductInfo :: ProductsInfo -> ProductsInfo -> MergedProductInfo
        mergeProductInfo tupleX tupleY = 
            (matchProducts tupleX tupleY) ++ (addUniqeProduct (matchProducts tupleX tupleY) tupleY)


        matchProducts :: ProductsInfo -> ProductsInfo -> MergedProductInfo
        matchProducts [] _ = []
        matchProducts (x:xs) webList =
            matchProductsAux x webList ++ (matchProducts xs webList)

        
        matchProductsAux :: SingleProductInfo -> ProductsInfo -> MergedProductInfo
        matchProductsAux (a,b) [] = [(a,b,"--")]
        matchProductsAux (a,b) (y:ys) =
            if a == fst y then [(a,b,snd y)]
            else matchProductsAux (a,b) ys


        addUniqeProduct :: MergedProductInfo-> ProductsInfo -> MergedProductInfo
        addUniqeProduct webList [] = []
        addUniqeProduct webList (y:ys) =
            (addUniqeProductAux webList y ) ++ (addUniqeProduct webList ys)

        
        addUniqeProductAux :: MergedProductInfo -> SingleProductInfo -> MergedProductInfo
        addUniqeProductAux [] y@(ay,by) = [(ay,"--",by)]
        addUniqeProductAux ((ax,bx,cx):xs) y@(ay,by)
            | ax == ay = []
            | otherwise = addUniqeProductAux xs y




        outputToFile :: String -> WebInfo -> WebInfo -> IO ()
        outputToFile search tuple1 tuple2 = do
            writeFile (search ++ ".csv" ) (firstTitle ++ ";" ++ (fst tuple1) ++ ";" ++ (fst tuple2) ++ "\n")
            appendFile (search ++ ".csv") (createCsvText (mergeProductInfo (snd tuple1) (snd tuple2)))
