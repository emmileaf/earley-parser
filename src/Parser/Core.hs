module Parser.Core where

import Data.HashMap.Strict as H (HashMap, empty, insert)
import Debug.Trace
import Grammar.Core

-- ===================================
-- Definitions for Parser Construction
-- ===================================

{--
A parser contains a list of statesets
A stateset holds a list of records
A record extends a production rule to also contain a dot (position),
pointer (to return after completion), and lookahead string
--}


-- Token
{--
A token is the unit consumed by the parser,
    usually representing a character or substring (word)
--}
data Token = Token String
    deriving (Show, Eq)

-- tokenize is a simple converter for an input string into character tokens

tokenize :: String -> [Token]
tokenize s = (map (\c -> Token [c]) s)

{--
readChar reads the ith token (x_i) of input string into a symbol
Returns: error symbol if token is out of bounds,
    or not found as a valid symbol in the provided grammar
--}
readChar :: Grammar -> [Token] -> Int -> Sym
readChar g input i
    | i < (length input) =
        let
            (Token c) = (input!!i)
         in if (Term c) `elem` (terminals g)
                then (Term c)
                else (Error ("Token " ++ c ++ " not valid in grammar"))
    | i == (length input) = RightTerm
    | otherwise = (Error ("Token index " ++ (show i) ++ " out of bounds"))

-- Record
{--
A record extends a production rule for use by the parser,
    and contains the following 4 elements:
     1) Production,
     2) Dot (position/index of production's RHS)
            (dot is before element [dot] in rhs)
     3) Pointer (index of set to return to)
     4) Lookahead (a symbol)
--}

data Rec = Rec { production :: Prod
                , dot :: Int
                , pointer :: Int
                , lookahead :: Sym
            }

instance Show Rec where
    show (Rec production dot pointer lookahead) =
        "Record: " ++ (show production) ++
        ", dot = " ++ (show dot) ++
        ", pointer = " ++ (show pointer) ++
        ", lookahead = " ++ (show lookahead) ++ "\n"

{--
Two records are equal if they have the same production,
dot position, and source stateset
--}

instance Eq Rec where
    (==) (Rec p1 d1 pt1 la1) (Rec p2 d2 pt2 la2) =
        ((p1 == p2) && (d1 == d2) && (la1 == la2) && (pt1 == pt2))


-- StateSet
{--
A stateset S_i (index si) is a linked list of records
    (to be processed in order by parser), =
    and also keeps track of token Xi as x (converted to Sym)
--}

data StateSet = StateSet { si :: Int, x :: Sym, records :: [Rec] }
    deriving (Eq)

instance Show StateSet where
    show (StateSet si t records) =
        "StateSet: S" ++ (show si) ++ "\n" ++
        "Symbol: X" ++ (show si) ++ " = " ++ (show t) ++ "\n" ++
        "Records: \n" ++ (unwords (map show records))

{--
Given a stateset and grammar, getRecords returns a map of all non-terminals
    to indexes of records which have dot before this non-terminal
--}
getRecords :: StateSet -> Grammar -> H.HashMap String [Int]
getRecords ss g =
    let
        recs = (records ss)
        nlst = (nonterminals g)
        result = (foldr
                    (\term hmap ->
                        H.insert
                        (toKey term)
                        (map
                            (\(r, i) -> i)
                            (filter
                                (\(r, i) ->
                                    ((length (rhs (production r))) > (dot r))
                                    && ((rhs (production r))!!(dot r) == term))
                                (zip recs [0..])))
                        hmap)
                    H.empty nlst)
     in result

-- Parser
{--
A running parser keeps track of
    1) List of StateSets
    2) The given Grammar
    3) Input tokens being parsed
    4) Index of current StateSet being processed
    5) Index of current Record in StateSet being processed

A FailedParser contains a message following an unsuccessful parse
--}
data Parser = Parser { ss :: [StateSet]
                    , grammar :: Grammar
                    , input :: [Token]
                    , currset :: Int
                    , currrec :: Int }
            | FailedParser String
        deriving (Eq)

instance Show Parser where
    show (Parser ss grammar input currset currrec) =
        "Parser: \n" ++
        "Input string: " ++ (show input) ++ "\n" ++
        "Current set: " ++ (show currset) ++ ", " ++
        "Current record: " ++ (show currrec) ++ "\n" ++
        "Statesets: \n" ++ (show ss) ++ "\n" ++
        "Map: \n" ++ (show (map (\s -> getRecords s grammar) ss)) ++ "\n" ++
        "Grammar: \n" ++ (show grammar)  ++ "\n"
    show (FailedParser s) = "Failed parse: " ++ s

{--
printParsed pretty-prints the parsed AST in (S (E a) (E a)) form,
    or the error message if parse was unsuccessful
--}
printParsed :: Parser -> String
printParsed (FailedParser s) = "(Parser failed: " ++ s ++ ")"
printParsed (Parser ss grammar input currset currrec) = let
                    success_rec = (head (records (last ss)))
                    result_prod = (production success_rec)
                 in "(" ++ (printExpr (lhs result_prod)) ++ ":" ++
                    (concatMap printExpr (rhs result_prod)) ++ ")"
