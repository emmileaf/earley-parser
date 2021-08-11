module Parser.Earley where

import Data.HashMap.Strict as H (HashMap, lookup)
import Data.List
import Debug.Trace

import Parser.Core
import Grammar.Core

-- =====================================
-- Running the Earley Parsing Algorithm
-- =====================================

----------------------------
-- Main Functions
----------------------------

{--
startParse builds a starting parser with the record
    S -> . R -|   0    -|
    where R is the root non-terminal of the grammar
--}

startParse :: Grammar -> String -> Parser
startParse g s =
    let
        srec = Rec { production=(Prod {lhs=Begin, rhs=[(root g), RightTerm]})
                    , dot=0
                    , pointer=0
                    , lookahead=RightTerm}
        sset = StateSet { si=0, x=Begin, records=[srec] }
    in Parser { ss=[sset], grammar=g, input=(tokenize s), currset=0, currrec=0 }

{--
checkParse checks whether the given parser has reached the success stateset
containing only the record
    S -> R -| .   0    -|
    where R is the root non-terminal of the grammar
--}
checkParse :: Parser -> Bool
checkParse p@(FailedParser f) = False
checkParse p@(Parser ss grammar input currset currrec) =
    let
        goalrec = Rec { production=(Prod {lhs=Begin,
                                          rhs=[(root grammar), RightTerm]})
                        , dot=2
                        , pointer=0
                        , lookahead=RightTerm}

        recs = records (last ss)
     in
        if ((length recs) == 1) && ((head recs) == goalrec)
        then True else False

{--
stepParse recursively performs one step of the parsing algorithm
    at a time, going through each record in each stateset,
    and calling one of the three actions (Predicter, Scanner, Completer)

stepParse returns a successful parser containing AST,
    or failed parser containing error message
--}

stepParse :: Parser -> Parser
stepParse p@(FailedParser f) = p
stepParse p@(Parser ss grammar input currset currrec) =
    let
        -- get the current record that parser is on
        recc = (records (ss!!currset))!!currrec

        -- get rhs of this record's production to check where the dot is
        prod_rhs = rhs (production recc)

        -- depending on dot position, take one of (scan/predict/complete) step
        p2 = if (length prod_rhs) == (dot recc)
                -- 1) dot at end of production, call completer
                then (completer p recc)
                else
                    let
                        next = prod_rhs!!(dot recc)
                        -- lookahead is partially implemented here
                        -- and not used by completer
                        lkhd = if (length prod_rhs) == ((dot recc) + 1)
                                    then (lookahead recc) -- reaches RHS end
                                    else (prod_rhs!!((dot recc) + 1))
                     in case next of
                         -- 2) non-terminal after dot, call predicter
                         (NonTerm n expr) -> (predicter p next lkhd)
                         -- 3) Terminal after dot, call scanner
                         (Term t) ->  (scanner p next recc)
                         -- 4) Right terminal after dot, call scanner like (3)
                         RightTerm ->  (scanner p next recc)

        -- Move on to next record in current stateset, or next stateset
        p3 = (nextStep p2)
     in
     -- Exit condition:
     -- Successful parse if (latest) set consists of the single success record
     -- Failed parse if next step out of bounds (no more records to process)
        case (checkParse p3) of
            True -> p3
            otherwise -> (stepParse p3)

----------------------------
-- Action Functions
-- Predict/Scan/Complete
----------------------------

-- A) Predicter
{--
predicter is called when a non-terminal is on the right of dot, and
adds a new record to Si (current) for each alternative of that non-terminal
    e.g. for `T -> . T * P -|  0  -|` in S0, predicter adds
             `T -> . P   0   *` and `T -> . T * P   0   *`

predicter takes a parser (p), the non-terminal right of dot (next),
    the lookahead symbol that comes after the non-terminal (lkhd),
    and returns an updated parser after taking a step of prediction

returns FailedParser if non-terminal is not found in grammar
--}
predicter :: Parser -> Sym -> Sym -> Parser
predicter p@(Parser ss grammar input currset currrec) next lkhd =
    let
        -- get alternative productions of the non-terminal next
        -- (assumption: the non-terminal next will not contain expanded subtree)
        nextprods = case (H.lookup (toKey next) (alts grammar)) of
            Just lst -> lst
            otherwise -> []
        -- Convert these alternatives to records with
            -- dot = at beginning of production
            -- lookahead = the symbol after next (in currrec's rhs)
            -- pointer = (currset, [currrec])
        -- and add to the current stateset Si
        nextrecs =  map (\pd -> Rec { production = pd
                                    , dot = 0
                                    , pointer = currset
                                    , lookahead = lkhd})
                    nextprods

        -- call addRecord for each of these records
        -- to add them (or update if duplicate of existing) correspondingly
        rnew = case nextrecs of
            [] -> (FailedParser ("Non-Terminal " ++ (show next)
                                ++ " not found in Grammmar"))
            otherwise ->
                (foldr (\record parser -> (addRecord record parser currset))
                        p
                        nextrecs)

     in rnew


-- B) Scanner
{--
scanner is called when a terminal is on the right of dot, and
    compares this terminal to the next token X_(i+1)
if they match, it adds a record to S_(i+1) (next stateset)
    with the dot shifted over that terminal
        e.g. for P -> . a   0   -| in S0, if next token is a,
             scanner makes a new stateset S1 and adds P -> a . 0  -|

scanner takes a parser (p), the terminal right of dot (next),
    the currently processing record (recc),
    and returns an updated parser after taking a step of scanning

returns FailedParser if scanning the next token fails
--}
scanner :: Parser -> Sym -> Rec -> Parser
scanner p@(Parser ss grammar input currset currrec) next recc =
    let
        -- read token X_i+1 into a terminal symbol
        nextx = readChar grammar input currset
    in case nextx of
        (Error s) -> (FailedParser s)
        otherwise ->
            if next == nextx
                -- terminal in record matches scanned symbol
                then let
                        -- make new record with dot shifted
                        newrec = Rec { production = (production recc)
                                    , dot = (dot recc) + 1
                                    , pointer = (pointer recc)
                                    , lookahead = (lookahead recc) }
                        -- add to stateset S_(i+1)
                        p2 = addRecord newrec p (currset + 1)

                      in p2
                else p

-- C) Completer
{--
completer is called when the end of a production is reached, and
goes back to the records given by pointer,
moves dot over the non-terminal, and add it as a new record to the current Si
    e.g. for P -> a .  2  -| in S3, completer goes to
    records 3 and 4 of stateset S2
    suppose they are T -> T * . P   0   -| and T -> . P   0   -|,
    then it moves the dot over P, adding
    T -> T * P .  0  -| and T -> P .  0  -| to S3

not implemented: to increase efficiency, the completer should also compare
    the lookahead string with the next token X_(i+1)
    and perform these actions only if they match

completer takes a parser (p), the currently processing record (recc),
and returns an updated parser after taking a step of scanning

returns FailedParser if fetching the next (lookahead) token fails
--}
completer :: Parser -> Rec -> Parser
completer p@(Parser ss grammar input currset currrec) recc =
    let
        expected = (lookahead recc)
        nextx = readChar grammar input currset -- X_i+1
        completed = (lhs (production recc))
     in case nextx of
        (Error s) -> (FailedParser s)
        otherwise ->
            let
                -- get source records using the source stateset's getRecords
                setp = ss!!(pointer recc)
                recmap = (getRecords setp grammar)
                recps = case (H.lookup (toKey completed) recmap) of
                            Just lst -> lst
                            otherwise -> []
                srecs = (map (\r -> (records setp)!!r) recps)
                -- make new records by moving dot over the non-terminal
                -- also add the parsed list of symbols to the non-terminal
                newrecs =
                    (map
                        (\srec ->
                            let
                                sprod = (production srec)
                                (NonTerm s expr) = (rhs sprod)!!(dot srec)
                                newsym = (NonTerm s (rhs (production recc)))
                                newrhs = map (\(i, sym) ->
                                                if i == (dot srec)
                                                    then newsym
                                                    else sym)
                                             (zip [0..] (rhs sprod))
                                newprod = Prod { lhs = (lhs sprod)
                                               , rhs=newrhs }
                             in Rec { production = newprod
                                    , dot = (dot srec) + 1
                                    , pointer = (pointer srec)
                                    , lookahead = (lookahead srec) }
                            )
                        srecs)
                -- add these records to the current stateset Si
                p2 = (foldr (\record parser ->
                                (addRecord record parser currset))
                            p
                            newrecs)
             in p2
             -- trace ("NonTerm " ++ (show (lhs (production recc))) ++
             -- " parsed as " ++ (show (rhs (production recc)))) p2

----------------------------
-- Helper Functions
----------------------------

{--
addRecord is a helper to add a Record (record) to a Parser (p)
    given a specified StateSet index (s)
if record already exists in the stateset, do not add again
if the specified stateset does not exist,
    create a new stateset and add as first record
--}
addRecord :: Rec -> Parser -> Int -> Parser
addRecord record p@(Parser ss grammar input currset currrec) s
    -- add to existing stateset
    | s < (length ss) =
        let
            targetset = ss!!s -- add to existing stateset
            newsets = if record `elem` (records targetset)
                        then ss
                        else
                            let
                                recs = (records targetset) ++ [record]
                                newss = StateSet { si=(si targetset),
                                                   x=(x targetset),
                                                   records=recs }
                                newsslist = map (\set ->
                                                    if (si set) == s
                                                        then newss
                                                        else set) ss
                             in newsslist
         in Parser { ss = newsets, grammar = grammar,
                     input = input, currset = currset, currrec = currrec }
     -- create next (new) stateset and add
     | s == (length ss) =
         let
             c = readChar grammar input (s - 1)
             snew = StateSet { si=s, x=c, records=[record] }
          in case c of
              (Error s) -> (FailedParser s)
              otherwise -> Parser { ss = (ss ++ [snew]), grammar = grammar,
                                    input = input, currset = currset,
                                    currrec = currrec }
     -- stateset specified is not existing or next
     | otherwise = (FailedParser ("Set index " ++ (show s) ++ " out of bounds"))

{--
nextStep is a helper that updates a Parser (p) to move onto the next record

Notes on behaviour:
it checks how many records are in the current stateset
    and increments to next record if not out of bound
otherwise (last record in stateset already processed),
    it checks how many statesets and moves onto first record in
    the next stateset if not out of bounds
--}
nextStep :: Parser -> Parser
nextStep p@(FailedParser f) = p
nextStep p@(Parser ss grammar input currset currrec) =
    let
        setc = ss!!currset
        (newset, newrec) =
            if (currrec + 1) < length (records setc)
                then (currset, currrec + 1) -- increment record
                else if currset + 1 < (length ss)
                        then (currset + 1, 0) -- increment stateset
                        else (-1, -1) -- out of bounds, handle below
     in case (newset, newrec) of
         (-1, -1) -> FailedParser "No more records, parse unsuccessful"
         otherwise -> Parser { ss = ss, grammar = grammar, input = input,
                               currset = newset, currrec = newrec }
