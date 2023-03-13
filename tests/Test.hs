{-# LANGUAGE OverloadedStrings #-}

import Control.Exception
import Test.Tasty
import Common
import Trie
import CallByName

main :: IO ()
main = runTests [ trie, callByValue ]

trie :: Score -> TestTree
trie sc = testGroup "Problem 1: Trie"
  [ scoreTest ( Trie.lookup "dog"
              , empty :: Trie String
              , Nothing
              , 2
              , "lookup 1" )
  , scoreTest ( Trie.lookup "dog"
              , sounds
              , Just "bow wow"
              , 2
              , "lookup 2")
  , scoreTest ( Trie.lookup "doge"
              , sounds
              , Just "so wow!"
              , 3
              , "lookup 3")
  , scoreTest ( Trie.lookup "doger"
              , sounds
              , Nothing
              , 3
              , "lookup 4")
  , scoreTest ( Trie.tmap (\k v -> if k == "car" then "VROOM" else v)
              , sounds
              , Node '*' [ Node 'c' [ Node 'a' [ Node 'r' [ Leaf "VROOM"]
                                   , Node 't' [ Leaf "meow" ] ]
                        , Node 'o' [ Node 'w' [ Leaf "moo"  ] ] ]
                         , Node 'd' [ Node 'o' [ Node 'g' [ Leaf "bow wow"
                                              , Node 'e' [ Leaf "so wow!"] ]
                                   , Node 'o' [ Node 'r' [ Leaf "creak"  ] ] ] ]
                         ]
              , 7
              , "tmap 1" )
  , scoreTest ( Trie.tmap (\k v -> v ++ " " ++ v)
              , sounds
              , Node '*' [ Node 'c' [ Node 'a' [ Node 'r' [ Leaf "vroom vroom"]
                                               , Node 't' [ Leaf "meow meow" ] ]
                                    , Node 'o' [ Node 'w' [ Leaf "moo moo"  ] ] ]
                         , Node 'd' [ Node 'o' [ Node 'g' [ Leaf "bow wow bow wow"
                                               , Node 'e' [ Leaf "so wow! so wow!"] ]
                                    , Node 'o' [ Node 'r' [ Leaf "creak creak"  ] ] ] ] 
             ]
              , 8
              , "tmap 2")
  , scoreTest  ( Trie.tfilter (\k v -> k == "car")
              , sounds
              , Node '*' [ Node 'c' [ Node 'a' [ Node 'r' [ Leaf "vroom"]]]]
              , 8
              , "tfilter 1")
  , scoreTest  ( Trie.tfilter (\k v -> v == "moo")
              , sounds
              , Node '*' [ Node 'c' [ Node 'o' [ Node 'w' [ Leaf "moo"]]]]
              , 8
              , "tfilter 2")
  , scoreTest  ( Trie.tfilter (\k v -> k == "car" && v == "moo")
              , sounds
              , empty :: Trie String
              , 9
              , "tfilter 3")
  ]
  where
    scoreTest :: (Show b, Eq b) => (a -> b, a, b, Int, String) -> TestTree
    scoreTest (f, x, r, n, msg) = scoreTest' sc (return . f, x, r, n, msg)

callByValue :: Score -> TestTree
callByValue sc = testGroup "Problem 2: Call By Value" [
  scoreTest  ( reduce1
              , EPlus (ENum 2) (ENum 3)
              , Just (ENum 5)
              , 4
              , "reduce1 1"),
  scoreTest  ( reduce1
              , ENum 5
              , Nothing
              , 4
              , "reduce1 2"),
  scoreTest  ( reduce1
              , EVar "cat"
              , Nothing
              , 4
              , "reduce1 3"),
  scoreTest  ( reduce1
              , EPlus (ENum 2) (ELam "x" (EVar "x"))
              , Nothing
              , 4
              , "reduce1 4")
  , scoreTest  ( reduceMany
              , (EApp (ELam "x" (EPlus "x" "x")) (ENum 3))
              , ENum 6
              , 4
              , "reduceMany 1")
  , scoreTest  ( reduceMany
              , e1
              , ENum 102
              , 5
              , "reduceMany 2")              
  , scoreTest  ( reduceMany
              , e4
              , ENum 3
              , 5
              , "reduceMany 3")              
  ]
  where
    scoreTest :: (Show b, Eq b) => (a -> b, a, b, Int, String) -> TestTree
    scoreTest (f, x, r, n, msg) = scoreTest' sc (return . f, x, r, n, msg)

e3 = ELet "h" (ELam "y" (EPlus "x" "y")) (EApp "f" "h")
e2 = ELet "x" (ENum 100) e3
e1 = ELet "f" (ELam "g" (ELet "x" (ENum 0) (EApp "g" (ENum 2)))) e2

e5 = EPlus "x" "y"
e4 = ELet "x" (ENum 1) (ELet "y" (ENum 2) e5)
