{-# LANGUAGE OverloadedStrings #-}

module CallByName where
import Data.String (IsString (fromString))
import Text.Printf (printf)

-- Definition of Nano2 syntax 

type Id = String

instance IsString Expr where
  fromString = EVar

data Expr  = ENum Int
           | EVar Id
           | EPlus Expr Expr
           | ELet Id   Expr  Expr
           | EApp Expr Expr
           | ELam Id   Expr
  deriving (Eq, Show)

exprString :: Expr -> String
exprString (ENum i)       = printf "%d" i
exprString (EVar x)       = x
exprString (EPlus e1 e2)  = printf "(%s %s %s)" (show e1) ("+" :: String) (show e2)
exprString (ELet x e e')  = printf "let %s = %s in \n %s" x (show e) (show e')
exprString (EApp e1 e2)   = printf "(%s %s)" (show e1) (show e2)
exprString (ELam x e)     = printf "\\%s -> %s" x (show e)

-- *******************************************************************
-- *** Problem 2: CALL-BY-NAME SEMANTICS *****************************
-- *******************************************************************

{-

In the lecture notes, we gave the operational semantics rules for 
the call-by-value evaluation strategy for Nano2. In this problem, we
give you the ...


There will not necessarily be the same number of evaluation rules
as there were for call-by-value. Your rules should still be 
deterministic (you don't need to prove that this is the case).

First, the reduction rules for addition are unchanged:

               e1 => e1'       
[Add-L]   --------------------
          e1 + e2 => e1' + e2   

              e2 => e2'
[Add-R]   --------------------
          n1 + e2 => n1 + e2'
          
[Add]     n1 + n2 => n       where n == n1 + n2          
-}

-- | PROBLEM [15 grader points] --------------------------------------

-- Write call-by-name reduction rules for applications. In the slides
-- we had three rules [App-L], [App-R], and [App]. You will not 
-- necessarily have the same number of rules. You can call them 
-- what ever you want.
--
-- You should write your rules down in the multi-line string below
-- like we did in the example above.
-- Alternatively, if you would prefer to write on paper or on your
-- tablet, you can upload your answer in a file called 
-- `semantics_callbyname_applications.jpg` and note that in the 
-- string below.

semantics_callbyname_applications :: String
semantics_callbyname_applications = " \
\ TODO: \
\ "

-- | PROBLEM [15 grader points] --------------------------------------

-- Write call-by-name reduction rules for let bindings. In the slides
-- we had two rules [Let-Def] and [Let]. You will not 
-- necessarily have the same number of rules. You can call them 
-- what ever you want.
--
-- You should write your rules down in the multi-line string below
-- like we did in the example above.
-- Alternatively, if you would prefer to write on paper or on your
-- tablet, you can upload your answer in a file called 
-- `semantics_callbyname_lets.jpg` and note that in the 
-- string below.

semantics_callbyname_lets :: String
semantics_callbyname_lets = " \
\  TODO:  \
\ "

-- | PROBLEM [30 public points, 30 private ] -------------------------

-- Our next task is to implement your call-by-name operational 
-- operational semantics above in terms of a function that takes a 
-- term `e` as input and returns `Just e'` if there exists a term
-- `e'` such that `e => e'` in the call-by-name semantics above
-- and `Nothing` otherwise.

-- Now, not every Expr can be reduced by one step; the terms that we
-- cannot reduce are the values and the stuck terms. So that means 
-- that `reduce1` may fail. Unlike Java, we cannot use null in Haskell.
-- Instead Haskell provides a built-in datatype `Maybe` to allow for the 
-- absence of a value or a function that may fail. It is defined as
--
--   data Maybe a = Nothing | Just a
--
-- Thus our `reduce1` function will have output type Maybe Expr. For 
-- instance:
--     * `reduce1 (ENum 5)` should return Nothing because (ENum 5)
--       is a value.
--     * `reduce1 (EBin Plus (ENum 2) (ENum 2))` should return 
--       `Just (ENum 4)`.
--
-- For stuck terms, like those that are ill-typed 
-- (e.g. `EApp (ENum 1) (Num 2)`) or those with undefined variables 
-- (e.g `EVar "cat"`) `reduce1` should return Nothing instead of 
-- throwing an error.
--
-- Don't worry about recursive function definitions.
--
-- This `reduce1 e` function should be deterministic: there should be
-- at most one `e'` that `e` can reduce to in one step. If you find 
-- that more than one rule could apply when implementing this function,
-- then be sure to correct your rules above.

reduce1 :: Expr -> Maybe Expr
reduce1 = error "TBD:eval"

-- We give you a function here to evaluate an expression `e` all the 
-- way to a value (or a stuck state), which is useful for phrasing
-- test cases. Note that the recursion here isn't well-founded
-- so this function can loop forever on some `e`.

reduceMany :: Expr -> Expr
reduceMany e = case (reduce1 e) of
  Nothing   -> e
  (Just e') -> reduceMany e'

-- We give you the code for capture-avoiding substitution here.

subst :: Id -> Expr -> Expr -> Expr
subst x v e = error "IMPLEMENT BEFORE RELEASE"

-- Here are some test cases!
--
-- >>> reduce1  (EPlus (ENum 2) (ENum 3))
-- Just (ENum 5)
--
-- >>> reduce1  (ENum 5)
-- Nothing
--
-- >>> reduce1  (EVar "cat")
-- Nothing
--
-- >>> reduce1  (EPlus (ENum 2) (ELam "x" (EVar "x")))
-- Nothing

-- TODO: add other test cases in terms of eval.

-- | PROBLEM [10 grader points] -------------------------

-- One of the differences between call-by-name and call-by-value is
-- that some terms whose reduction never terminates under
-- call-by-value will terminate under call-by-name. Give us an
-- example of a term `e_cbn` such that `reduceMany e_cbn` will
-- terminate and return a value under our semantics but would loop
-- forever under the call-by-value semantics.

e_cbn :: Expr
e_cbn = error "TBD: e_cbn"

