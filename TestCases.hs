import Crawler (komplettCrawler, komplettSearch, komplettPairNmPr, komplettPrice, komplettName, priceAndNamePairer, inetSearch, inetPrice, inetName, inetPair)
import PrintPrice 
import Menu (mergeText)
import Test.HUnit


runtests = runTestTT $ TestList [test1,test2,test3,test4,test5,test6,test7,test8,test9,test10,test11,test12,test13,test14,test15,test16,test17,test18,test19,test20]

-------------------------TESTS FOR CRAWLER-----------------
{-
Some mock html codes used for test cases
-}
testHTMLInet :: String
testHTMLInet = "<!DOCTYPE html> <html> <span class='price'>11 119</span> <h4>MSI GeForce RTX 3070 Ti 8GB GAMING X TRIO</h4> </html>"

testHTMLKomplett :: String
testHTMLKomplett = " <!DOCTYPE html> <html> <span class='product-price-now'>11 199</span> <h2>MSI GeForce RTX 3070 Ti GAMING X TRIO</h2> </html>"

test1 = TestCase $ assertEqual "komplettCrawler"
  [("MSI GeForce RTX 3070 Ti GAMING X TRIO","11 199")] (komplettCrawler "MSI" "3070" testHTMLKomplett)

test2 = TestCase $ assertEqual  "komplettSearch" 
  [("foobar","baz")] (komplettSearch [("foo","foo"),("bar","bar"),("foobar","baz")] "foo" "bar" [])

test3 = TestCase $ assertEqual "komplettPairNmPr"
  [("MSI GeForce RTX 3070 Ti GAMING X TRIO","11 199")] (komplettPairNmPr testHTMLKomplett)

test4 = TestCase $ assertEqual "komplettPrice"
  ["11 199"] (komplettPrice testHTMLKomplett)

test5 = TestCase $ assertEqual "komplettName"
  ["MSI GeForce RTX 3070 Ti GAMING X TRIO"] (komplettName testHTMLKomplett)

test6 = TestCase $ assertEqual "priceAndNamePairer"
  [("foo","bar")] (priceAndNamePairer ["foo"] ["bar"] [])

test7 = TestCase $ assertEqual "inetSearch"
  [("foo","foo"),("foobar","baz")] (inetSearch [("foo","foo"),("bar","bar"),("foobar","baz")] "foo" [])

test8 = TestCase $ assertEqual "inetPrice"
  ["11 119"] (inetPrice testHTMLInet)

test9 = TestCase $ assertEqual "inetName" 
  ["MSI GeForce RTX 3070 Ti 8GB GAMING X TRIO"] (inetName testHTMLInet)

test10 = TestCase $ assertEqual "inetPair"
  [("foo","bar")] (inetPair ["foo"] ["bar"])

------------------TESTS FOR MENU------------------------

test11 = TestCase $ assertEqual "mergeText"
  "foo bar" (mergeText "foo" "bar")

------------------TESTS FOR PRINTPRICE---------------

{-
Some mock productlists used for test cases
-}
komplettTestList :: (String, [(String, String)])
komplettTestList = ("Komplett",[("rtx3080" , "10000"), ("3090" , "20000")])
inetTestList :: (String, [(String, String)])
inetTestList= ("Inet",[("rtx 3060", "1000"),("rtx 3070","5000"),("rtx3080", "15000")])

test12 = TestCase $ assertEqual "createCsvText"
  "foo;bar;baz\n" (createCsvText [("foo","bar","baz")])

test13 = TestCase $ assertEqual "createCsvText"
  "" (createCsvText [])

test14 = TestCase $ assertEqual "addUniqeProductAux"
  [] (addUniqeProductAux [("foo","bar","baz")] ("foo","bar"))

test15 = TestCase $ assertEqual "addUniqeProductAux"
  [("baz","--","bar")] (addUniqeProductAux [("foo","bar","baz")] ("baz","bar"))

test16 = TestCase $ assertEqual "addUniqeProduct"
  [("test1","--","test2")] (addUniqeProduct [("foo","bar","baz")] [("test1","test2")])

test17 = TestCase $ assertEqual "addUniqeProduct"
  [] (addUniqeProduct [("foo","bar","baz")] [])

test18 = TestCase $ assertEqual "matchProductsAux"
  [("foo","bar","baz")] (matchProductsAux ("foo","bar") [("bar","baz"),("foo","baz")])

test19 = TestCase $ assertEqual "matchProducts"
  [("foo","bar","baz")] (matchProducts [("foo","bar")] [("foo","baz")])

test20 = TestCase $ assertEqual "mergeProductInfo"
 [("rtx 3060","1000","--"),("rtx 3070","5000","--"),("rtx3080","15000","10000"),("3090","--","20000")] (mergeProductInfo (snd inetTestList) (snd komplettTestList))