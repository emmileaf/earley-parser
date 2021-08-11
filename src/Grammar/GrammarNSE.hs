module Grammar.GrammarNSE where

import Grammar.Core
----------------------------
-- The NSE Grammar
----------------------------

-- Grammar NSE:
-- R -> A B
-- A -> a
-- A -> R C
-- B -> b
-- B -> D B
-- C -> c
-- D -> d
--
-- Terminals: ['a', 'b', 'c', 'd']
-- Non-Terminals: ['R', 'A', 'B', 'C', 'D']
--
-- Example of valid input string: "adbcddb"

nseprods = [(Prod {lhs=(NonTerm "R" []),
                  rhs=[(NonTerm "A" []), (NonTerm "B" [])]})
        , (Prod {lhs=(NonTerm "A" []),
                 rhs=[(Term "a")]})
        , (Prod {lhs=(NonTerm "A" []),
                 rhs=[(NonTerm "R" []), (NonTerm "C" [])]})
        , (Prod {lhs=(NonTerm "B" []),
                 rhs=[(Term "b")]})
        , (Prod {lhs=(NonTerm "B" []),
                 rhs=[(NonTerm "D" []), (NonTerm "B" [])]})
        , (Prod {lhs=(NonTerm "C" []),
                 rhs=[(Term "c")]})
        , (Prod {lhs=(NonTerm "D" []),
                 rhs=[(Term "d")]})]

nseterms = [(Term "a"), (Term "b"), (Term "c"), (Term "d")]
nsenonterms = [(NonTerm "R" []), (NonTerm "A" []), (NonTerm "B" []),
                (NonTerm "C" []), (NonTerm "D" [])]
nseroot = (NonTerm "R" [])

nseg = Grammar { productions=nseprods
                , terminals=nseterms
                , nonterminals=nsenonterms
                , root=nseroot }
