
{-
------------------------------------------------
PROGRAMKONSTRUKTION & DATASTRUKTURER HT2020
ASSIGNMENT 1

Tim Solig 
Viktor Hultsten
------------------------------------------------
-}



-- DO NOT MODIFY THE FOLLOWING LINE
module CompLing(wordCount, adjacentPairs, pairsCount, neighbours, mostCommonNeighbour) where

import Test.HUnit -- provides testing framework
import PandP      -- provide sample text to play with (variable austin)

-- DO NOT CHANGE THESE TYPES
type Sentence = [String]
type Document = [Sentence]
type WordTally = [(String, Int)]
type Pairs = [(String, String)]
type PairsTally = [((String, String), Int)]

-- DO NOT CHANGE THE TYPE SIGNATURES FOR THESE FUNCTIONS




------------------------------------- UPPGIFT 1 -------------------------------------

{- wordCount arguments
     Computes a tally of all the distinct words appearing in the document.
     PRE: 
     RETURNS: 
     EXAMPLES: 
-}


--FINNS DET NÅGOT SÄTT ATT SLIPPA wordCountAcc? Att slå ihop den med wordCount? obs "concat doc"
wordCount :: Document -> WordTally
wordCount doc = wordCountAcc (concat doc)

wordCountAcc :: Sentence -> WordTally

wordCountAcc [] = []
wordCountAcc doc@(x:xs) = 
     countElement x doc : wordCountAcc (filter (/= x) doc)


countElement :: (Eq a) => a -> [a] -> (a, Int)
countElement e lst = countElementAcc e 0 lst 



countElementAcc :: (Eq a) => a -> Int -> [a] -> (a, Int)
countElementAcc e acc [] = (e, acc)
countElementAcc e acc (x:xs)
     | e == x = countElementAcc e (acc + 1) xs
     | otherwise = countElementAcc e acc xs



------------------------------------- UPPGIFT 2 -------------------------------------

{- adjacentPairs document
     Yields a list of all adjacent pairs of words appearing in the document, with duplicates present.
     RETURNS: A list of all adjacent pairs of words, including duplicates
     EXAMPLES: 
          adjacentPairs [["This", "is", "an", "example"],["It", "includes", "two", "sentences"]]
                         ==   [("This","is"),("is","an"),("an","example"),("It","includes"),("includes","two"),("two","sentences")]
          
          adjacentPairs [["time", "for", "a", "break"], ["not", "for", "a", "while"]]
                         == [("time","for"),("for","a"),("a","break"),("not","for"),("for","a"),("a","while")]
          
          adjacentPairs [] == []
-}

adjacentPairs :: Document -> Pairs
adjacentPairs [] = []
adjacentPairs (x:xs) = foo x ++ adjacentPairs xs
     where 
          foo :: [String] -> Pairs
          foo [] = []
          foo [x] = []
          foo (x:xs) = (x, head xs) : foo xs





------------------------------------- UPPGIFT 3 -------------------------------------

{- initialPairs Document
     Creates a list of all pairs of words appearing at the start of sentences in the document, with duplicates present.
     RETURNS: list of inital two words of each sentence, given that the sentence has more than one word
     EXAMPLES: 
          initialPairs [["but","it","is"],["returned","she"]] == [("but","it"),("returned","she")]
          initialPairs [["but","it","is"],["returned","she"],["wow"],["this","works","indeed"]] == [("but","it"),("returned","she"),("this","works")]
-}
initialPairs :: Document -> Pairs
--VARIANT: length Document
initialPairs [] = []
initialPairs ([_]:xs) = initialPairs xs
initialPairs (x:xs) = (head x, x !! 1) : initialPairs xs


{- finalPairs arguments
     A list of all pairs of words appearing at the end of sentences in the document, with duplicates present.
     PRE:  
     RETURNS: 
     SIDE EFFECTS: 
     EXAMPLES: 
-}
finalPairs :: Document -> Pairs
--VARIANT: length Document
finalPairs [] = []
finalPairs ([_]:xs) = finalPairs xs
finalPairs (x:xs) = (x !! (length x -2), x !! (length x - 1)) : finalPairs xs




------------------------------------- UPPGIFT 4 -------------------------------------

{- pairsCount arguments
     Computes a tally of all pairs.
     PRE:       
     RETURNS: 
     SIDE EFFECTS: 
     EXAMPLES:
-}

pairsCount :: Pairs -> PairsTally
--VARIANT: length Pairs
pairsCount [] = []
pairsCount (x:xs) = countPairs x (x:xs) : pairsCount [elem | elem <- xs, not (isEqual elem x)]

countPairs :: (Eq a) => (a, a) -> [(a,a)] -> ((a,a), Int)
countPairs e lst = countPairsAcc e 0 lst
     where
          countPairsAcc :: (Eq a, Num t) => (a, a) -> t -> [(a, a)] -> ((a, a), t)
          countPairsAcc e acc [] = (e, acc)
          countPairsAcc e acc (x:xs)
               | isEqual e x = countPairsAcc e (acc + 1) xs
               | otherwise = countPairsAcc e acc xs

isEqual :: (Eq a) => (a, a) -> (a, a) -> Bool
isEqual (a,b) (c,d) = (a == c && b == d) || (a == d && b == c)





------------------------------------- UPPGIFT 5 -------------------------------------

{- neighbours arguments
     Takes  a  tally  of pairs, such as computed by the pairsCount function, 
     and a word and gives all the words that appear with that word in the tally of pairs along with the number of occurrences.
     PRE:  
     RETURNS: 
     SIDE EFFECTS: 
     EXAMPLES: 
-}
neighbours :: PairsTally -> String -> WordTally
neighbours [] _ = []
neighbours (((a,b),n):xs) str
     | a == str = (b, n) : neighbours xs str
     | b == str = (a, n) : neighbours xs str
     | otherwise = neighbours xs str





------------------------------------- UPPGIFT 6 -------------------------------------

{- mostCommonNeighbhour arguments
     Returns the word that occurs most frequently with a given word, based on a tally of pairs.
     PRE:  
     RETURNS: 
     SIDE EFFECTS:
     EXAMPLES: 
-}
mostCommonNeighbour :: PairsTally -> String -> Maybe String
mostCommonNeighbour tally str = searchFun str tally 0 ""

searchFun :: String -> PairsTally -> Int -> String -> Maybe String 
searchFun _ [] 0 _ = Nothing
searchFun _ [] _ maxWord = Just maxWord
searchFun str (((a,b),num):xs) maxNum maxWord
     | str == a && num > maxNum = searchFun str xs num b
     | str == b && num > maxNum = searchFun str xs num a
     | otherwise = searchFun str xs maxNum maxWord





------------------------------------- TEST -------------------------------------
-- Test Cases
-- feel free to add other test cases here an independent set of
-- test cases will be used when grading your code

-- wordCount
test1 = TestCase $ assertEqual "wordCount []" [] (wordCount [])
test2 = TestCase $ assertBool "wordCount [[\"a\",\"b\"],[\"a\"]]" (elem ("a",2) (wordCount [["a","b"],["a"]]))

-- adjacentPairs, initialPairs, finalPairs
test3 = TestCase $ assertEqual "adjacentPairs [[\"foo\"],[\"bar\"]]" [] (adjacentPairs [["foo"],["bar"]]) 

test3a = TestCase $ assertEqual "initialPairs" [("a","b")] (initialPairs [["a","b","a"],["c"]])
                      
test3b = TestCase $ assertEqual "finalPairs" [("b","a")] (finalPairs [["a","b","a"],["c"]])
                      
-- pairsCount
test4 = TestCase $ assertBool "pairsCount simple" 
            (elem (("a","b"), 2) (pairsCount [("a","b"),("c","d"),("a","b")]))
test5 = TestCase $ assertBool "pairsCount tricky" 
             (let x = pairsCount (adjacentPairs [["a","b","a"],["c"]]) in 
                      elem (("a","b"), 2) x || elem (("b","a"), 2) x)

-- neighbours
test6 = TestCase $ assertEqual "neighbours left" [("b",2)] 
                                                 (neighbours [(("a","b"),2),(("c","d"),1)] "a") 

test7 = TestCase $ assertEqual "neighbours left" [("a",2)]
                                                 (neighbours [(("a","b"),2),(("c","d"),1)] "b") 

-- mostCommonNeighbour
test8 = TestCase $ assertEqual "mostCommonNeighbour text \"the\"" (Just "fun") 
                                                                  (mostCommonNeighbour input "the") 
  where input = [(("the", "fun"),4),(("the","foot"),3),(("dog","power"),2)]

test9 = TestCase $ assertEqual "mostCommonNeighbour text \"spam\"" 
                      Nothing (mostCommonNeighbour input "spam")
  where input = [(("the", "fun"),4),(("the","foot"),3),(("dog","power"),2)]

-- testing the PandP.austin text
test10 = TestCase $ assertEqual "mostCommonNeighbour of \"bennet\"" 
            (Just "mr") (mostCommonNeighbour (pairsCount $ adjacentPairs $ austin) "bennet") 

-- for running all the tests (type "runtests" within ghci --- without the quotes)
runtests = runTestTT $ TestList [test1, test2, test3, test3a, test3b, test4, test5, test6, test7,test8,test9,test10]




