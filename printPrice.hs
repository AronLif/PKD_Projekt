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


        {-  createCsvText ((ax,bx,cx):xs)
            This functions creates a string with speciall characthers so that the CSV file will know 
            how to separate the items.
            PRE:        True
            RETURNS:    A String with all the data collected and modified for the CSV file
            VARIANT:    length of recived list     
            EXAMPLES:   createCsvText [("foo","bar","foo++"),("test1","test2","test3")]
                        "foo;bar;foo++\ntest1;test2;test3\n"
        -}
        createCsvText :: MergedProductInfo -> CsvInfo
        createCsvText [] = []
        createCsvText ((ax,bx,cx):xs) = 
            ax ++ ";" ++ bx ++ ";" ++ cx ++ "\n" ++ createCsvText xs

        
        {-  mergeProductInfo listX listY 
            This function matches and merges the data in the two lists of tuples.
            If one tuple has a product that the first tuple don't have then it will add "--" by the price section
            PRE:        True
            RETURNS:    A singe list with tuples with the price data merged
            EXAMPLES:   mergeProductInfo [("rtx2070","3000")] [("rtx2070","5000"),("rtx 2060","2000")]
                        [("rtx2070","3000","5000"),("rtx 2060","--","2000")]
        -}       
        mergeProductInfo :: ProductsInfo -> ProductsInfo -> MergedProductInfo
        mergeProductInfo listX listY = 
            (matchProducts listX listY) ++ (addUniqueProducts (matchProducts listX listY) listY)


        {-  matchProducts (x:xs) webList
            This function takes two lists and calls on a helper function to match all the items in the first list with items in the second list.
            if and item exists in both list it will add the prices of both lists. if and items only exists in the first list the price will be "--"
            where the second list would have had it'äs price
            PRE:        True
            RETURNS:    A list of tuples with [(item,price1,price2)]
            VARIANT:    Length of (x:xs)
            EXAMPLES:   matchProducts [("rtx2070","2000"),("rtx2060","3000")] [("rtx2070","2500"),("rtx2050","1000")]
                        [("rtx2070","2000","2500"),("rtx2060","3000","--")]
        -}
        matchProducts :: ProductsInfo -> ProductsInfo -> MergedProductInfo
        matchProducts [] _ = []
        matchProducts (x:xs) webList =
            matchProductsAux x webList ++ (matchProducts xs webList)


        {-  matchProductsAux (a,b) (y:ys)
            This function checks if the value a exists in (y:ys). 
            if it does it returns the price in (y:ys) that is assosicated with a. = [(a,b,y-price)]
            if the a doesn't exist in (y:ys) it will return "--" instead of y-price = [(a,b,"--")]
            PRE:        True
            RETURNS:    a list with a single tuple containing the data as stated above
            VARIANT:    (y:ys)
            EXAMPLES:   matchProductsAux  ("rtx2070","2000") [("rtx2070","2500"),("rtx2050","1000")]
                        [("rtx2070","2000","2500")]
        -}
        matchProductsAux :: SingleProductInfo -> ProductsInfo -> MergedProductInfo
        matchProductsAux (a,b) [] = [(a,b,"--")]
        matchProductsAux (a,b) (y:ys) =
            if a == fst y then [(a,b,snd y)]
            else matchProductsAux (a,b) ys


        {-  addUniqueProducts weblist (y:ys)
            this functions calls on a helper that returns all the items in the (y:ys) list that aren't in the mergedList
            PRE:        True
            RETURNS:    A list of all items in (y:ys) list that aren't in the mergedList
            VARIANT:    length of (y:ys)
            EXAMPLES:   addUniqueProducts [("rtx2070","2000","2500")] [("rtx2070","2500"),("rtx2050","1000")]
                        [("rtx2050","--","1000")]
        -}
        addUniqueProducts :: MergedProductInfo-> ProductsInfo -> MergedProductInfo
        addUniqueProducts mergedList [] = []
        addUniqueProducts mergedList (y:ys) =
            (addUniqueProductsAux mergedList y ) ++ (addUniqueProducts mergedList ys) 


        {-  addUniqueProductsAux ((ax,bx,cx):xs) y@(ay,by)
            this function checks if the ay value exists in the ((ax,bx,cx):xs) list.
            if it does it returns []
            if the ay doesn't exist in the ((ax,bx,cx):xs) list it will return = [(ay,"--",by)]
            PRE:        True
            RETURNS:    a list
            VARIANT:    length of ((ax,bx,cx):xs)
            EXAMPLES:   addUniqueProductsAux [("rtx2070","2000","2500")] ("rtx2050","2000")
                        [("rtx2050","--","2000")]
        -} 
        addUniqueProductsAux :: MergedProductInfo -> SingleProductInfo -> MergedProductInfo
        addUniqueProductsAux [] y@(ay,by) = [(ay,"--",by)]
        addUniqueProductsAux ((ax,bx,cx):xs) y@(ay,by)
            | ax == ay = []
            | otherwise = addUniqueProductsAux xs y



        {-  outputToFile search tuple1 tuple2
            
            PRE:            
            RETURNS:        
            SIDE EFFECTS:   
            VARIANT:        
            EXAMPLES:            
        -} 
        outputToFile :: String -> WebInfo -> WebInfo -> IO ()
        outputToFile search tuple1 tuple2 = do
            writeFile (search ++ ".csv" ) (firstTitle ++ ";" ++ (fst tuple1) ++ ";" ++ (fst tuple2) ++ "\n")
            appendFile (search ++ ".csv") (createCsvText (mergeProductInfo (snd tuple1) (snd tuple2)))
