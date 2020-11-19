{-
------------------------------------------------
PROGRAMKONSTRUKTION & DATASTRUKTURER HT2020
ASSIGNMENT 1

Tim Solig 
Viktor Hultsten
------------------------------------------------
-}



{-GITBHUB INSTRUCTION
 **Stage changes with "+"

 **Write message

 **Commit with "✔"

 **Push!

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


{- wordCount arguments
     Computes a tally of all the distinct words appearing in the document.
     PRE: 
     RETURNS: 
     SIDE EFFECTS: 
     EXAMPLES: 
-}

wordCount :: Document -> WordTally
wordCount doc = wordCountAcc (concat doc)

wordCountAcc :: Sentence -> WordTally
wordCountAcc [] = []
wordCountAcc doc@(x:xs) = 
     countElement x doc : wordCountAcc (removeElement x doc)




-- wordCount'' :: Document -> WordTally
-- wordCount'' [] = []
-- wordCount'' doc = wordCount'' [removeElement (head doc) doc] ++ [(head doc), countElement (head doc) doc)]


{-
removeElement :: String -> Sentence -> Sentence
removeElement _ [] = []
removeElement e (x:xs) 
     | e == x = removeElement e xs
     | otherwise = x : removeElement e xs -}


removeElement :: String -> Sentence -> Sentence
removeElement el lst = filter (/= el) lst



countElement :: (Eq a) => a -> [a] -> (a, Int)
-- countElement :: String -> Sentence -> (String, Int)
countElement e lst = countElementAcc e 0 lst 

countElementAcc :: (Eq a) => a -> Int -> [a] -> (a, Int)
-- countElementAcc :: String -> Int -> Sentence -> (String, Int)
countElementAcc e acc [] = (e, acc)
countElementAcc e acc (x:xs)
     | e == x = countElementAcc e (acc + 1) xs
     | otherwise = countElementAcc e acc xs





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

--KLAR--






{- initialPairs Document
     Creates a list of all pairs of words appearing at the start of sentences in the document, with duplicates present.
     PRE: length of sentences in Document are greather than 1 (solved)
     RETURNS: 
     SIDE EFFECTS: 
     EXAMPLES: 
-}
initialPairs :: Document -> Pairs
initialPairs [] = []
initialPairs ([_]:xs) = initialPairs xs
initialPairs (x:xs) = (head x, x !! 1) : initialPairs xs
--KLAR--







{- finalPairs arguments
     A list of all pairs of words appearing at the end of sentences in the document, with duplicates present.
     PRE:  
     RETURNS: 
     SIDE EFFECTS: 
     EXAMPLES: 
-}
finalPairs :: Document -> Pairs
finalPairs [] = []
-- finalParis (x:lst) = initialPairs (reverse x) ++ finalPairs lst
finalPairs (x:lst) = foo7 x ++ finalPairs lst
     where
          foo7 :: Sentence -> Pairs
          foo7 [] = []
          foo7 lst
               | length lst == 1 = []
               | otherwise = [(lst !! (length lst -2), lst !! (length lst - 1))]
--KLAR--











{- pairsCount arguments
     Computes a tally of all pairs.
     PRE:       
     RETURNS: 
     SIDE EFFECTS: 
     EXAMPLES:
-}

pairsCount :: Pairs -> PairsTally
pairsCount = undefined

countElement' :: (Eq a) => (a, a) -> [(a,a)] -> ((a,a), Int)
countElement' e lst = countElementAcc' e 0 lst 

-- countElementAcc' :: (Eq a) => (a, a) -> Int -> [a] -> (a, Int)
-- countElementAcc :: String -> Int -> Sentence -> (String, Int)
countElementAcc' :: (Eq a, Num t) => (a, a) -> t -> [(a, a)] -> ((a, a), t)
countElementAcc' e acc [] = (e, acc)
countElementAcc' e acc (x:xs)
     | isEqual e x = countElementAcc' e (acc + 1) xs
     | otherwise = countElementAcc' e acc xs


--LEVEL TVÅ - IDÈ GÖRA OM ISEQUAL OCH COUNTACC FÖR BÅDA!!

-- isEqual :: (Eq a) => (a, a) -> (a, a) -> Bool
isEqual (a,b) (c,d) = (a==c && b == d) || (a == d && b == c)



--[("hej","da"), ("da","heja"), ("hej","da"), ("hej","da"), ("hej","da"), ("hej","da"), ("hej","da"), ("hej","da"), ("hej","da")]














{- neighbours arguments
     Takes  a  tally  of pairs, such as computed by the pairsCount function, 
     and a word and gives all the wordsthat appear with that word in the tally of pairs along with the number of occurrences.
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











{- mostCommonNeighbhour arguments
     Returns the word that occurs most frequently with a given word, based on a tally of pairs.
     PRE:  
     RETURNS: 
     SIDE EFFECTS:
     EXAMPLES: 
-}
mostCommonNeighbour :: PairsTally -> String -> Maybe String
mostCommonNeighbour = undefined  -- remove "undefined" and write your function here
























-- Test Cases
-- feel free to add other test cases here. an independent set of
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




