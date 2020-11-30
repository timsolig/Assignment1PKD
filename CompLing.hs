{-
------------------------------------------------
PROGRAM CONSTRCUTION & DATA STRUCTURES HT2020
ASSIGNMENT 1

Tim Solig
Viktor Hultsten
------------------------------------------------
-}

module CompLing (wordCount, adjacentPairs, pairsCount, neighbours, mostCommonNeighbour) where

import PandP
import Test.HUnit

{- Sentence
     Represents a sentence, i.e words in a specific order.
     Words are strings. The type gathers words (in a sentence) in a list.
-}
type Sentence = [String]

{- Document
     Represents a Document, i.e sentences in a specific order.
     The typ gathers sentences in a list.
-}
type Document = [Sentence]

{- WordTally
     Represents the occurences of strings.
     A list of tuples (pairs) where the first and second element is the string and the occurence (int) respectively.
-}
type WordTally = [(String, Int)]

{- Pairs
     Represents pair of words (Strings).
     A list of tuples (pairs) where the first and second element are strings.

-}
type Pairs = [(String, String)]

{- PairsTally
     Represents the occurence of Pairs .
     A list of tuples (pairs) where the first and second element is the pair and the occurence (int) respectively.
-}
type PairsTally = [((String, String), Int)]






------------------------------------- PROBLEM 1 -------------------------------------

{- wordCount doc
     Computes a tally of all the distinct words appearing in the document.
     RETURNS: First and second element are the words from "doc" and their number of occurrences in "doc" respectively.
     EXAMPLES:
          wordCount [["is","he","married","or","single"],["oh"]] == [("is",1),("he",1),("married",1),("or",1),("single",1),("oh",1)]
          wordCount [["This","is","an","example"],["An","example","that","works"]] == [("This",1),("is",1),("an",1),("example",2),("An",1),("that",1),("works",1)]
-}
wordCount :: Document -> WordTally
wordCount doc = wordCountAcc (concat doc)



{- wordCountAcc concatedDoc
     Computes a tally of all the distinct words appearing in the document.
     RETURNS: First and second element are the words from "concatedDoc" and their number of occurrences in "concatedDoc" respectively.
     EXAMPLES:
          wordCountAcc ["is","he","married","or","single","oh"] == [("is",1),("he",1),("married",1),("or",1),("single",1),("oh",1)]
          wordCountAcc ["This","is","an","example","an","example","that","works"] == [("This",1),("is",1),("an",1),("example",2),("an",1),("that",1),("works",1)]
-}
wordCountAcc :: [String] -> WordTally
--VARIANT: length concatedDoc
wordCountAcc [] = []
wordCountAcc doc@(x:_) =
  countElement x doc : wordCountAcc (filter (/= x) doc)



{- countElement element list
    Gives the number of times an element occurs in a given list.
    RETURNS: First and second element are "element" and occurence of "element" in "list" respectiely.
    EXAMPLES:
         countElement "a" ["this","is","a","list","with","a","element"] == ("a",2)
         countElement 1 [1,2,3,4] == (1,1)
         countElement (1,2) [(1,2),(2,3)] == ((1,2),1)
 -}
countElement :: (Eq a) => a -> [a] -> (a, Int)
countElement e lst = countElementOccurrence e 0 lst
  where
    {- countElementOccurrence element occurrences list
        Counts the occurrences of an element in a list
        RETURNS: First and second element are "element" and "occurrences" of "element" in "list" respectively.
        EXAMPLES: countElementOccurrence "a" 0 ["this","is","a","list","with","a","element"] == ("a",2)
                  countElementOccurrence 1 0 [1,2,3,4] == (1,1)
                  countElementOccurrence (1,2) 0 [(1,2),(2,3)] == ((1,2),1)
    -}
    countElementOccurrence :: (Eq a) => a -> Int -> [a] -> (a, Int)
    --VARIANT: length list
    countElementOccurrence e occ [] = (e, occ)
    countElementOccurrence e occ (x : xs)
      | e == x = countElementOccurrence e (occ + 1) xs
      | otherwise = countElementOccurrence e occ xs

------------------------------------------------------------------------------------






------------------------------------- PROBLEM 2 -------------------------------------

{- adjacentPairs document
     Yields a list of all adjacent pairs of words appearing in a document, with duplicates present.
     RETURNS: Adjacent pairs of words in "document", including duplicates.
     EXAMPLES:
          adjacentPairs [["This", "is", "an", "example"],["It", "includes", "two", "sentences"]]
                         ==   [("This","is"),("is","an"),("an","example"),("It","includes"),("includes","two"),("two","sentences")]

          adjacentPairs [["time", "for", "a", "break"], ["not", "for", "a", "while"]]
                         == [("time","for"),("for","a"),("a","break"),("not","for"),("for","a"),("a","while")]

          adjacentPairs [] == []
-}
adjacentPairs :: Document -> Pairs
--VARIANT: length document
adjacentPairs [] = []
adjacentPairs (x : xs) = pairsOfList x ++ adjacentPairs xs
  where
    {- pairsOfList sentence
         Returns all adjacent pairs of words appearing in a sentence, with duplicates present. 
         RETURNS: Adjacent pairs of words in "sentence", including duplicates.
         EXAMPLES:
              pairsOfList ["This","is","an","example"] == [("This", "is"), ("is", "an"),("an", "example")]
    -}
    pairsOfList :: [String] -> Pairs
    --VARIANT: length sentence
    pairsOfList [] = []
    pairsOfList [_] = []
    pairsOfList (x1 : x2 : xs) = (x1, x2) : pairsOfList (x2 : xs)

------------------------------------------------------------------------------------






------------------------------------- PROBLEM 3 -------------------------------------

{- initialPairs doc
     Creates a list of all pairs of words appearing at the start of sentences of a document, with duplicates present.
     RETURNS: Inital two words of each sentence of "doc", given that the sentence has more than one word
     EXAMPLES:
          initialPairs [["but","it","is"],["returned","she"]] == [("but","it"),("returned","she")]
          initialPairs [["but","it","is"],["returned","she"],["wow"],["this","works","indeed"]] == [("but","it"),("returned","she"),("this","works")]
-}
initialPairs :: Document -> Pairs
--VARIANT: length doc
initialPairs [] = []
initialPairs ([_] : xs) = initialPairs xs
initialPairs ((y : z : _) : xs) = (y, z) : initialPairs xs



{- finalPairs doc
     Gives a list of all last two words appearing in the end of sentences of a document, with duplicates present.
     RETURNS: All pairs of words appearing at the end of each sentence of "doc", given that the sentence has more than one word.
     EXAMPLES:
          finalPairs [["this","is","a","sentence"], ["wow"], ["this","is","another","sentence"]] == [("a","sentence"),("another","sentence")]
-}
finalPairs :: Document -> Pairs
--VARIANT: length doc
finalPairs [] = []
finalPairs ([_] : xs) = finalPairs xs
finalPairs (x : xs) = (x !! (length x -2), x !! (length x - 1)) : finalPairs xs

------------------------------------------------------------------------------------






------------------------------------- PROBLEM 4 -------------------------------------
{- pairsCount pairs
     Counts and computes a tally of pairs.
     RETURNS: Pairs in "pairs" and their number of occurrences respectively.
     EXAMPLES:
          pairsCount [("a", "pair"),("another", "pair"),("pair","a")] == [(("a","pair"),2),(("another","pair"),1)]
-}
pairsCount :: Pairs -> PairsTally 
--VARIANT: length pairs
pairsCount [] = []
pairsCount (x : xs) = pairsCountAcc x 0 (x : xs) : pairsCount [elem | elem <- xs, not (isEqual elem x)]
  where
    {- pairsCountAcc element accumulator list
         Counts number of occurrences of element in list, internal order independent.
         PRE: accumulator == 0
         RETURNS: First and second elements are "element" and  the number of its' occurences, "accumulator" that is, respectively
         EXAMPLES:
              pairsCountAcc ("a", "pair") 0 [("a", "pair"),("another", "pair"),("pair","a")] == (("a","pair"),2)
              pairsCountAcc ("a", "pair") 0 [] == (("a","pair"),0)
    -}
    pairsCountAcc :: (Eq a) => (a, a) -> Int -> [(a, a)] -> ((a, a), Int)
    --VARIANT: length list
    pairsCountAcc e acc [] = (e, acc)
    pairsCountAcc e acc (x : xs)
      | isEqual e x = pairsCountAcc e (acc + 1) xs
      | otherwise = pairsCountAcc e acc xs



{- isEqual tuple1 tuple2
     Disregarding the order, determines if two tuples have the same elements.
     RETURNS: True if both elements in tuple1 are also in tuple2, False otherwise.
     EXAMPLES:
          isEqual (1,2) (2,1) == True
          isEqual ("big","bear") ("bear","big") == True
          isEqual ("no","way") ("yes","indeed") == False
  -}
isEqual :: (Eq a) => (a, a) -> (a, a) -> Bool
isEqual (a, b) (c, d) = (a == c && b == d) || (a == d && b == c)

------------------------------------------------------------------------------------






------------------------------------- PROBLEM 5 -------------------------------------

{- neighbours tally string
     Gives all the words which are neighbours of a given word and the number of times that occurs.
     RETURNS: Neighbours of "string" and its' number of occurences in "tally" respectively.
     EXAMPLES: neighbours [(("bear","big"),2),(("big","dog"),1)] "big" == [("bear", 2),("dog",1)]
               neighbours [(("bear","big"),2),(("big","dog"),1),(("bear","dog"),5)] "big" == [("bear", 2),("dog",1)]
               neighbours [] "big" == []
-}
neighbours :: PairsTally -> String -> WordTally
--VARIANT: length tally
neighbours [] _ = []
neighbours (((a, b), n) : xs) str
  | a == str = (b, n) : neighbours xs str
  | b == str = (a, n) : neighbours xs str
  | otherwise = neighbours xs str

------------------------------------------------------------------------------------






------------------------------------- PROBLEM 6 -------------------------------------

{- mostCommonNeighbour tally string
     Gives the word which is the most common neighbour of a word in a tally.
     RETURNS: the most common neighbour to "string" in "tally". Nothing if it does not occur
     EXAMPLES: mostCommonNeighbour [(("bear","big"),2),(("big","dog"),1)] "big" == Just "bear"
               mostCommonNeighbour [(("bear","big"),1),(("big","dog"),5),(("dog","bear"),50)] "big" == Just "dog"
               mostCommonNeighbour [] "boy" == Nothing
-}
mostCommonNeighbour :: PairsTally -> String -> Maybe String
mostCommonNeighbour tally str = mostCommonNeighbourAcc str tally 0 ""
  where
    {- mostCommonNeighbourAcc tally string maxFreq maxWord
        Gives the word which is the most common neighbour of a word in a tally.
        PRE: maxFreq
        RETURNS: the neighbour "maxWord" with highest occurrency "maxFreq", to "string" in "tally". Nothing if it does not occur.
        EXAMPLES:
              mostCommonNeighbourAcc "big" [(("bear","big"),1),(("big","dog"),5),(("dog","bear"),50)] 0 "" == Just "dog"
              mostCommonNeighbourAcc "bear" [(("bear","big"),1),(("big","dog"),5),(("dog","bear"),50)] 0 "" == Just "dog"
    -}
    mostCommonNeighbourAcc :: String -> PairsTally -> Int -> String -> Maybe String
    --VARIANT: length tally
    mostCommonNeighbourAcc _ [] 0 _ = Nothing
    mostCommonNeighbourAcc _ [] _ maxWord = Just maxWord
    mostCommonNeighbourAcc str (((a, b), num) : xs) maxNum maxWord
      | str == a && num > maxNum = mostCommonNeighbourAcc str xs num b
      | str == b && num > maxNum = mostCommonNeighbourAcc str xs num a
      | otherwise = mostCommonNeighbourAcc str xs maxNum maxWord

------------------------------------------------------------------------------------





------------------------------------- TEST -------------------------------------
-- Test Cases
-- feel free to add other test cases here an independent set of
-- test cases will be used when grading your code

-- wordCount
test1 = TestCase $ assertEqual "wordCount []" [] (wordCount [])

test2 = TestCase $ assertBool "wordCount [[\"a\",\"b\"],[\"a\"]]" (elem ("a", 2) (wordCount [["a", "b"], ["a"]]))

-- adjacentPairs, initialPairs, finalPairs
test3 = TestCase $ assertEqual "adjacentPairs [[\"foo\"],[\"bar\"]]" [] (adjacentPairs [["foo"], ["bar"]])

test3a = TestCase $ assertEqual "initialPairs" [("a", "b")] (initialPairs [["a", "b", "a"], ["c"]])

test3b = TestCase $ assertEqual "finalPairs" [("b", "a")] (finalPairs [["a", "b", "a"], ["c"]])

-- pairsCount
test4 =
  TestCase $
    assertBool
      "pairsCount simple"
      (elem (("a", "b"), 2) (pairsCount [("a", "b"), ("c", "d"), ("a", "b")]))

test5 =
  TestCase $
    assertBool
      "pairsCount tricky"
      ( let x = pairsCount (adjacentPairs [["a", "b", "a"], ["c"]])
         in elem (("a", "b"), 2) x || elem (("b", "a"), 2) x
      )

-- neighbours
test6 =
  TestCase $
    assertEqual
      "neighbours left"
      [("b", 2)]
      (neighbours [(("a", "b"), 2), (("c", "d"), 1)] "a")

test7 =
  TestCase $
    assertEqual
      "neighbours left"
      [("a", 2)]
      (neighbours [(("a", "b"), 2), (("c", "d"), 1)] "b")

-- mostCommonNeighbour
test8 =
  TestCase $
    assertEqual
      "mostCommonNeighbour text \"the\""
      (Just "fun")
      (mostCommonNeighbour input "the")
  where
    input = [(("the", "fun"), 4), (("the", "foot"), 3), (("dog", "power"), 2)]

test9 =
  TestCase $
    assertEqual
      "mostCommonNeighbour text \"spam\""
      Nothing
      (mostCommonNeighbour input "spam")
  where
    input = [(("the", "fun"), 4), (("the", "foot"), 3), (("dog", "power"), 2)]

-- testing the PandP.austin text
test10 =
  TestCase $
    assertEqual
      "mostCommonNeighbour of \"bennet\""
      (Just "mr")
      (mostCommonNeighbour (pairsCount $ adjacentPairs $ austin) "bennet")

-- for running all the tests (type "runtests" within ghci --- without the quotes)
runtests = runTestTT $ TestList [test1, test2, test3, test3a, test3b, test4, test5, test6, test7, test8, test9, test10]
