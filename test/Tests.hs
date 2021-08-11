{-# OPTIONS_GHC -F -pgmF htfpp #-}
module Tests (htf_thisModulesTests) where

import Test.Framework
import Data.HashMap.Strict as H (HashMap, fromList)

import Parser.Core
import Parser.Earley
import Grammar.Core
import Grammar.GrammarAE
import Grammar.GrammarNSE
import Mocks

-------------------------------------
-- (Integration) Testing the Parser
-------------------------------------
parse :: Grammar -> String -> String
parse grammar input = (printParsed (stepParse (startParse grammar input)))

test_parser_aeg = do
                    assertEqual (parse aeg "a") "( S: (E (T (P a))) -|)"
                    assertEqual (parse aeg "a+a") "( S: (E (E (T (P a))) + (T (P a))) -|)"
                    assertEqual (parse aeg "a*a") "( S: (E (T (T (P a)) * (P a))) -|)"
                    assertEqual (parse aeg "a+a*a") "( S: (E (E (T (P a))) + (T (T (P a)) * (P a))) -|)"
                    assertEqual (parse aeg "a++" ) "(Parser failed: No more records, parse unsuccessful)"
                    assertEqual (parse aeg "b") "(Parser failed: Token b not valid in grammar)"

test_parser_nseg = do
                       assertEqual (parse nseg "ab") "( S: (R (A a) (B b)) -|)"
                       assertEqual (parse nseg "abcdb") "( S: (R (A (R (A a) (B b)) (C c)) (B (D d) (B b))) -|)"
                       assertEqual (parse nseg "adbcddb") "( S: (R (A (R (A a) (B (D d) (B b))) (C c)) (B (D d) (B (D d) (B b)))) -|)"
                       assertEqual (parse nseg "ae") "(Parser failed: Token e not valid in grammar)"
                       assertEqual (parse nseg "abcd") "(Parser failed: No more records, parse unsuccessful)"

-----------------------------------
-- (Unit) Testing Grammar.Core
-----------------------------------
test_toKey = do
                assertEqual (toKey (Term "a")) "a"
                assertEqual (toKey (NonTerm "A" [])) "A"
                assertEqual (toKey (NonTerm "B" [(Term "b")])) "B"
                assertEqual (toKey Begin) "Begin"
                assertEqual (toKey RightTerm) "RightTerm"

test_printExpr = do
                assertEqual (toKey (Term "a")) "a"
                assertEqual (toKey (NonTerm "A" [])) "A"
                assertEqual (toKey (NonTerm "B" [(Term "b")])) "B"
                assertEqual (toKey Begin) "Begin"
                assertEqual (toKey RightTerm) "RightTerm"

test_alts = do
                assertEqual (alts aeg) (H.fromList
                                        [("P", [ Prod {lhs = (NonTerm "P" []), rhs = [(Term "a")]} ])
                                        ,("T", [ Prod {lhs = (NonTerm "T" []), rhs = [(NonTerm "P" [])]}
                                                ,Prod {lhs = (NonTerm "T" []), rhs = [(NonTerm "T" []),(Term "*"),(NonTerm "P" [])]}])
                                        ,("E", [ Prod {lhs = (NonTerm "E" []), rhs = ([NonTerm "T" []])}
                                                ,Prod {lhs = (NonTerm "E" []), rhs = [(NonTerm "E" []),(Term "+"),(NonTerm "T" [])]}])])
                assertEqual (alts nseg) (H.fromList
                                        [("R", [ Prod {lhs = (NonTerm "R" []), rhs = [(NonTerm "A" []),(NonTerm "B" [])]} ])
                                        ,("A", [ Prod {lhs = (NonTerm "A" []), rhs = [(Term "a")]}
                                                ,Prod {lhs = (NonTerm "A" []), rhs = [(NonTerm "R" []),(NonTerm "C" [])]} ])
                                        ,("B", [ Prod {lhs = (NonTerm "B" []), rhs = [(Term "b")]}
                                                ,Prod {lhs = (NonTerm "B" []), rhs = [(NonTerm "D" []),(NonTerm "B" [])]} ])
                                        ,("C", [ Prod {lhs = (NonTerm "C" []), rhs = [(Term "c")]} ])
                                        ,("D", [ Prod {lhs = (NonTerm "D" []), rhs = [(Term "d")]} ])])

-----------------------------------
-- (Unit) Testing Parser.Core
-----------------------------------

test_tokenize = do
                    assertEqual (tokenize "abc") [(Token "a"), (Token "b"), (Token "c")]
                    assertEqual (tokenize "a+c") [(Token "a"), (Token "+"), (Token "c")]
                    assertEqual (tokenize "a") [(Token "a")]
                    assertEqual (tokenize "") []

test_readChar = do
                    assertEqual (readChar aeg [(Token "a"), (Token "b"), (Token "c")] 0) (Term "a")
                    assertEqual (readChar aeg [(Token "a"), (Token "b"), (Token "c")] 1) (Error "Token b not valid in grammar")
                    assertEqual (readChar aeg [(Token "a"), (Token "b"), (Token "c")] 4) (Error "Token index 4 out of bounds")
                    assertEqual (readChar nseg [(Token "a"), (Token "b"), (Token "c")] 1) (Term "b")
                    assertEqual (readChar nseg [(Token "a"), (Token "b"), (Token "+")] 2) (Error "Token + not valid in grammar")

test_getRecords = do
                        assertEqual (getRecords set1 nseg) (H.fromList [("R", []), ("A", [0,1]), ("B", []), ("C", [2]), ("D", [])])
                        assertEqual (getRecords set2 aeg) (H.fromList [("E", []), ("T", [0]), ("P", [1,2])])

-- Covered by integration testing: printParsed :: Parser -> String

-----------------------------------
-- (Feature) Testing Parser.Earley
-----------------------------------

-- Main functions:

test_startParse = do
                    assertEqual (startParse aeg "ab") startp_aeg
                    assertEqual (startParse nseg "ba") startp_nseg

test_checkParse = do
                    assertEqual (checkParse startp_aeg) False
                    assertEqual (checkParse startp_nseg) False
                    assertEqual (checkParse failedp) False
                    assertEqual (checkParse successp_aeg) True
                    assertEqual (checkParse successp_nseg) True

-- Covered by integration testing: stepParse :: Parser -> Parser


-- Helper functions:

test_addRecord = do
                    assertEqual (addRecord rec_existing_aeg predict_aeg 0) predict_aeg
                    assertEqual (addRecord rec_existing_nseg predict_nseg 0) predict_nseg
                    assertEqual (addRecord rec_existing_aeg predict_aeg 1) predict_add1_aeg
                    assertEqual (addRecord rec_existing_nseg predict_nseg 1) predict_add1_nseg
                    assertEqual (addRecord rec_existing_aeg predict_aeg 2) failedp2
                    assertEqual (addRecord rec_existing_nseg predict_nseg 2) failedp2
                    assertEqual (addRecord rec_new_aeg predict_aeg 0) predict_add0_aeg
                    assertEqual (addRecord rec_new_nseg predict_nseg 0) predict_add0_nseg

test_nextStep = do
                    assertEqual (nextStep startp_aeg) failedp
                    assertEqual (nextStep startp_nseg) failedp
                    assertEqual (nextStep predict_aeg) predict_next_aeg
                    assertEqual (nextStep predict_nseg) predict_next_nseg

-- Action functions:

test_predicter = do
                    assertEqual (predicter startp_aeg (NonTerm "E" []) RightTerm) predict_aeg
                    assertEqual (predicter startp_nseg (NonTerm "R" []) RightTerm) predict_nseg

test_completer = do
                    assertEqual (completer complete_before_aeg ((records ((ss complete_before_aeg)!!1))!!0) ) complete_after_aeg

test_scanner = do
                    assertEqual (scanner scan_before_aeg (Term "a") ((records ((ss scan_before_aeg)!!0))!!1) ) scan_after_aeg
                    assertEqual (scanner scan_nomatch_aeg (Term "a") ((records ((ss scan_nomatch_aeg)!!0))!!1) ) scan_nomatch_aeg
