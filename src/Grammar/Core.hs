module Grammar.Core where

import Data.HashMap.Strict as H (HashMap, empty, insert)
import Data.List
import Debug.Trace

-- =================================
-- Definition for Grammars
-- Including symbols and productions
-- =================================


-- Grammar
{--
A grammar (context-free) consists of
1) productions
2) terminals
3) nonterminals
4) root nonterminal (must be one of nonterminals)
--}

data Grammar = Grammar { productions :: [Prod]
                        , terminals :: [Sym]
                        , nonterminals :: [Sym]
                        , root :: Sym
                    } deriving (Show, Eq)

-- Symbol
{--
A symbol is either a Terminal (or the special right terminator)
    or a Non-Terminal (or the special starting symbol),
    or an error symbol returned with a message when readChar fails on a token
For construction of the AST expression (see below), each non-terminal
    may also hold its expanded subtree as a list of symbols
--}
data Sym = Term String | NonTerm String [Sym] | RightTerm | Begin | Error String
    deriving (Show)

instance Eq Sym where
    (==) (Term a) (Term b) = (a == b)
    (==) (NonTerm a e1) (NonTerm b e2) = (a == b)
    (==) RightTerm RightTerm = True
    (==) Begin Begin = True
    (==) (Error a) (Error b) = (a == b)
    (==) _ _ = False

-- toKey extracts the string represented by the symbol to use as key in hashmap
toKey :: Sym -> String
toKey (NonTerm c expr) = c
toKey (Term c) = c
toKey sym = (show sym)

-- printExpr is used by printParsed for printing a parsed AST in (S (E a)) form
printExpr :: Sym -> String
printExpr (NonTerm c expr) = " (" ++ c ++ (concatMap printExpr expr) ++ ")"
printExpr (Term c) = " " ++ c
printExpr Begin = " S"
printExpr RightTerm = " -|"
printExpr (Error s) = s

-- Production
-- A production has LHS (Non-Terminal) and RHS (List of symbols)

data Prod = Prod { lhs :: Sym, rhs :: [Sym] } deriving (Show, Eq)

-- For each non-terminal, alts return a list of its alternatives
-- (productions with this non-terminal as LHS) for use by the predicter

type Alts = H.HashMap String [Prod]

alts :: Grammar -> Alts
alts g@(Grammar prods terms nonterms root) =
    (foldr
        (\term hmap -> H.insert
                        (toKey term)
                        (filter (\p -> (lhs p) == term) prods)
                        hmap)
        H.empty nonterms)
