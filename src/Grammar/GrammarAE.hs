module Grammar.GrammarAE where

import Grammar.Core
----------------------------
-- The AE Grammar
----------------------------

-- Grammar AE:
-- E -> T
-- E -> E + T
-- T -> P
-- T -> T * P
-- P -> a
--
-- Terminals: ['a', '+', '*']
-- Non-Terminals: ['E', 'T', 'P']
--
-- Example of valid input string: "a+a*a"

aeprods = [(Prod {lhs=(NonTerm "E" []),
                  rhs=[(NonTerm "T" [])]})
        , (Prod {lhs=(NonTerm "E" []),
                 rhs=[(NonTerm "E" []), (Term "+"), (NonTerm "T" [])]})
        , (Prod {lhs=(NonTerm "T" []),
                 rhs=[(NonTerm "P" [])]})
        , (Prod {lhs=(NonTerm "T" []),
                 rhs=[(NonTerm "T" []), (Term "*"), (NonTerm "P" [])]})
        , (Prod {lhs=(NonTerm "P" []),
                 rhs=[(Term "a")]})]

aeterms = [(Term "+"), (Term "*"), (Term "a")]
aenonterms = [(NonTerm "E" []), (NonTerm "T" []), (NonTerm "P" [])]
aeroot = (NonTerm "E" [])

aeg = Grammar { productions=aeprods
                , terminals=aeterms
                , nonterminals=aenonterms
                , root=aeroot }
