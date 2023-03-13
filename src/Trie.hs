
module Trie where 
    
import Prelude hiding (lookup)

-- *****************************************************************************
-- *** Problem 1 : TRIES *******************************************************
-- *****************************************************************************

{-  Key-Value tables are pretty slow when implemented as linked lists. 
    A hash-table backed implementation would be faster, but there's still
    some memory inefficiency if we're storing very many similar keys.
 
    Lets develop a faster lookup-data structure called a "Trie". Our keys
    will always be Strings, but the values can be anything.

    Concretely consider a map from *keys* which are always strings to *sounds*:

    car  -> "vroom"
    cat  -> "meow"
    cow  -> "moo"
    dog  -> "bow wow"
    doge -> "so wow"
    door -> "creak"

    There is a fair amount of **sharing** between the *keys*.
    We can compactly represent the above map pictorially as
    a *Trie*  which would look like:

    *- c - a - r -> "vroom"
    |  |   |
    |  |    `- t -> "meow"
    |  |
    |   `- o - w -> "moo"
    |    
    `- d - o - g -> "bow wow"
           |   |
           |    `- e -> "so wow!"
           |
            `- o - r -> "creak"

    Note that each internal node is labeled with exactly one Char
    except for the root node which must be unlabelled (else all of our 
    key strings would have to start with the same letter). Each
    leaf is labelled with a value that we're storing in the trie.

    In order to implement the root node being unlabeled as a special 
    case we will take the Char '*' to signify the node being unlabeled
    which is not considered part of any key. You can assume that the 
    special Char '*' can only appear at the root node of a trie and 
    nowhere else.

    You can assume that every internal Node's children satisfy the
    following invariants:
      * Every Node must have at least one child (except possibly for
        the root node labeled '*').
      * At most one child of a Node is a Leaf (i.e. each key appears
        at most once in the trie).
      * All the children of each Node will have distinct characters,
        for instance a Node labeled 'c' cannot have two children
        labeled 'a'.
    Let's develop some higher order functions on tries.

    First the definition of trie:
-}

-- | Representing Tries -------------------------------------------------

data Trie a = Node Char [Trie a]
            | Leaf a
  deriving (Eq, Show)

-- The example Trie shown above would be represented as

sounds :: Trie String
sounds = 
    Node '*' [ Node 'c' [ Node 'a' [ Node 'r' [ Leaf "vroom"]
                                   , Node 't' [ Leaf "meow" ] ]
                        , Node 'o' [ Node 'w' [ Leaf "moo"  ] ] ]
             , Node 'd' [ Node 'o' [ Node 'g' [ Leaf "bow wow"
                                              , Node 'e' [ Leaf "so wow!"] ] 
                                   , Node 'o' [ Node 'r' [ Leaf "creak"  ] ] ] ]
             ]

-- An `empty` Trie

empty :: Trie a
empty = Node '*' []

-- | WARMUP PROBLEM [10 public points, 10 private points] ----------------------

-- | 'lookup cs t' takes a `[Char]` named `cs` and a `Trie a` named `t` and 
--   returns 'Just v' where 'v' is the value associated with `cs` in `t` 
--   if it exists, and `Nothing` otherwise. Recall the definition of Maybe a:
--
--   data Maybe a = Nothing 
--                | Just a
--
-- When you are done you should get this behavior:
--
-- >>> lookup "dog" empty
-- Nothing
--
-- >>> lookup "dog" sounds
-- Just "bow wow"
--
-- >>> lookup "doge" sounds
-- Just "so wow!"
--
-- >>> lookup "doger" sounds 
-- Nothing
--
-- HINT: The helper functions `getVal` and `getKid` shown below may be useful!
-- 
lookup :: [Char] -> Trie a -> Maybe a 
-- We are out of letters to search up and we're not at a node
lookup [] (Leaf _) = Nothing
-- we finished looking up and we're at a node - if it has a value, return it
lookup [] (Node _ kids) = getValFromKids kids
  where
    getValFromKids :: [Trie a] -> Maybe a
    getValFromKids [] = Nothing
    getValFromKids ((Leaf x) : _) = Just x
    getValFromKids (_:xs) = getValFromKids xs
-- we have letters to look up. Look for a kid with that value
-- and search on remaining letters
lookup (c:cs) t = case getKid c t of
  Just trie -> lookup cs trie
  Nothing -> Nothing

-- | 'getVal t' returns 'Just v' if trie 't' is a Leaf with
--   value 'v' at the root.
getVal :: Trie a -> Maybe a
getVal (Leaf x)  = Just x
getVal _         = Nothing 

-- | 'getKid c t' returns 'Just ct' if 'ct' is the child of 't' 
--   with 'c'  at its root, or it returns 'Nothing' if no such 
--   sub-tree exists. We can assume that no Node in a Trie ever
--   has more than one child with the same label
getKid :: Char -> Trie a -> Maybe (Trie a)
getKid c (Node _ cts) = go cts
  where 
    go (ct:cts) = case ct of
      (Node c' _)  -> if c == c' then Just ct else go cts
      (Leaf _)     -> go cts
    go []             = Nothing
getKid c (Leaf _)     = Nothing

-- | PROBLEM [15 public points, 15 private points] -----------------------------

-- | 'tmap f t' is a `map` for tries. `tmap` takes a function 
--   `f :: [Char] -> a -> b` (that acts on both the key and value)
--   and a trie `t :: Trie a` and outputs another trie of type
--   `Trie b` that contains all of the same keys as `t` but has 
--   values obtained by applying `f k v` for every key-value pair.

tmap :: ([Char] -> a -> b) -> Trie a -> Trie b 
tmap f (Node '*' kids) = Node '*' (traverseKids "" kids)
  where
    traverseKids _ [] = []
    -- first parameter is the key so far. apply the function to leaves...
    traverseKids key (Leaf x : xs) = Leaf (f key x) : traverseKids key xs
    -- and if we hit a node, add that letter to the key and apply to kids, and keep traversing
    traverseKids key (Node k kids : xs) = Node k (traverseKids (key ++ [k]) kids) : traverseKids key xs

tmap _ _ = error "invalid trie"

-- When you are done you should get this behavior:

-- >>> tmap (\k v -> if k == "car" then "VROOM" else v) sounds
--               , Node '*' [ Node 'c' [ Node 'a' [ Node 'r' [ Leaf "VROOM"]
--                                     , Node 't' [ Leaf "meow" ] ]
--                          , Node 'o' [ Node 'w' [ Leaf "moo"  ] ] ]
--                          , Node 'd' [ Node 'o' [ Node 'g' [ Leaf "bow wow"
--                                                           , Node 'e' [ Leaf "so wow!"] ] ]
--                                                , Node 'o' [ Node 'r' [ Leaf "creak"  ] ] ] 
--                ]
--  

-- >>> tmap (\k v -> v ++ " " ++ v) sounds
-- Node '*' [ Node 'c' [ Node 'a' [ Node 'r' [ Leaf "vroom vroom"]
--                                , Node 't' [ Leaf "meow meow" ] ]
--                     , Node 'o' [ Node 'w' [ Leaf "moo moo"  ] ] ]
--          , Node 'd' [ Node 'o' [ Node 'g' [ Leaf "bow wow bow wow"
--                                           , Node 'e' [ Leaf "so wow! so wow!"] ] ]
--                                , Node 'o' [ Node 'r' [ Leaf "creak creak"  ] ] ] 
--          ]

-- | PROBLEM [25 public points, 25 private points] -----------------------------

-- | 'tfilter p t' is a `filter` for tries. `tfilter` takes a predicate
--   `p :: String -> a -> Bool` (that acts on both the key and value)
--   and a trie `t :: Trie a` and outputs another trie of type
--   `Trie a` that contains only those key-value pairs that satisfy the
--   predicate `p`.

--   Your answer should preserve the general structure of the trie.
--   Remember the invariant:
--    * Every Node must have at least one child.

tfilter :: ([Char] -> a -> Bool) -> Trie a -> Trie a
-- forces that starting node
tfilter f (Node '*' kids) = Node '*' (traverseKids "" kids)
  where
    traverseKids _ [] = []

    -- if we have a leaf, only include it if it matches condition
    traverseKids key (Leaf v : xs)
      | f key v = Leaf v : traverseKids key xs
      | otherwise = traverseKids key xs

    -- if we have a node, recursively call traverseKids on its kids
    -- if it now has no kids, do not include this node.
    traverseKids key (Node k kids : xs) = case traversedKids of
      [] -> traverseKids key xs
      _ -> Node k traversedKids : traverseKids key xs
      where
        traversedKids = traverseKids (key ++ [k]) kids

tfilter _ _ = error "invalid starting trie"

-- When you are done you should get this behavior:

-- >>> tfilter (\k v -> k == "car") sounds
-- Node '*' [ Node 'c' [ Node 'a' [ Node 'r' [ Leaf "vroom"]]]]

-- >>> tfilter (\k v -> v == "moo") sounds
-- Node '*' [ Node 'c' [ Node 'o' [ Node 'w' [ Leaf "moo"]]]]

-- >>> tfilter (\k v -> k == "car" && v == "moo") sounds
-- Node '*' []
