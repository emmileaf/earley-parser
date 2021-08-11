module Mocks where

import Parser.Core
import Grammar.Core
import Grammar.GrammarAE
import Grammar.GrammarNSE

------------------
-- StateSets
------------------

set1 = StateSet { si=0,
                  x=(Term "a"),
                  records=[
                            Rec { production=(Prod {lhs=(NonTerm "R" []), rhs=[(NonTerm "A" []), (NonTerm "B" [])]}),
                                  dot=0,
                                  pointer=0,
                                  lookahead=RightTerm},
                            Rec { production=(Prod {lhs=(NonTerm "R" []), rhs=[(NonTerm "A" []), (NonTerm "B" [])]}),
                                  dot=0,
                                  pointer=0,
                                  lookahead=(Term "c")},
                            Rec { production=(Prod {lhs=(NonTerm "A" []), rhs=[(NonTerm "R" []), (NonTerm "C" [])]}),
                                  dot=1,
                                  pointer=0,
                                  lookahead=(Term "c")}
                          ]
                }

set2 = StateSet { si=0,
                  x=(Term "a"),
                  records=[
                            Rec { production=(Prod {lhs=(NonTerm "E" []), rhs=[(NonTerm "E" []), (Term "+"), (NonTerm "T" [])]}),
                                  dot=2,
                                  pointer=0,
                                  lookahead=RightTerm},
                            Rec { production=(Prod {lhs=(NonTerm "T" []), rhs=[(NonTerm "T" []), (Term "*"), (NonTerm "P" [])]}),
                                  dot=2,
                                  pointer=0,
                                  lookahead=(Term "a")},
                            Rec { production=(Prod {lhs=(NonTerm "T" []), rhs=[(NonTerm "P" [])]}),
                                  dot=0,
                                  pointer=0,
                                  lookahead=(Term "a")}
                          ]
                }

------------------
-- Records
------------------

rec_existing_aeg = Rec { production=Prod {lhs=(NonTerm "E" []), rhs=[(NonTerm "T" [])]},
                         dot=0, pointer=0, lookahead=RightTerm }

rec_new_aeg = Rec { production=Prod {lhs=(NonTerm "E" []), rhs=[(NonTerm "E" []), (Term "+"), (NonTerm "T" [])]},
                         dot=1, pointer=0, lookahead=RightTerm }

rec_existing_nseg = Rec { production=Prod {lhs=(NonTerm "R" []), rhs=[(NonTerm "A" []), (NonTerm "B" [])]},
                                           dot=0, pointer=0, lookahead=RightTerm }

rec_diff_nseg = Rec { production=Prod {lhs=(NonTerm "R" []), rhs=[(NonTerm "A" []), (NonTerm "B" [])]},
                                          dot=0, pointer=0, lookahead=RightTerm }

rec_new_nseg = Rec { production=Prod {lhs=(NonTerm "A" []), rhs=[(Term "a")]},
                                        dot=0, pointer=0, lookahead=RightTerm }
------------------
-- Parsers
------------------

-- Note that many of these are simplified for purposes of unit-testing
-- and may not be valid parser states in an end-to-end run

failedp = FailedParser "No more records, parse unsuccessful"
failedp2 = FailedParser "Set index 2 out of bounds"

startp_aeg =
    Parser {
        ss=[StateSet { si=0, x=Begin,
                       records=[Rec { production=Prod {lhs=Begin, rhs=[(NonTerm "E" []), RightTerm]},
                                      dot=0, pointer=0, lookahead=RightTerm }] }],
        grammar=aeg,
        input=[(Token "a"), (Token "b")],
        currset=0,
        currrec=0
    }

startp_nseg =
    Parser {
        ss=[StateSet { si=0, x=Begin,
                       records=[Rec { production=Prod {lhs=Begin, rhs=[(NonTerm "R" []), RightTerm]},
                                      dot=0, pointer=0, lookahead=RightTerm }] }],
        grammar=nseg,
        input=[(Token "b"), (Token "a")],
        currset=0,
        currrec=0
    }

successp_aeg =
    Parser {
        ss=[StateSet { si=4, x=RightTerm,
                       records=[Rec { production=Prod {lhs=Begin, rhs=[(NonTerm "E" []), RightTerm]},
                                      dot=2, pointer=0, lookahead=RightTerm }] }],
        grammar=aeg,
        input=[(Token "a"), (Token "+"), (Token "a")],
        currset=4,
        currrec=0
    }

successp_nseg =
    Parser {
        ss=[StateSet { si=3, x=RightTerm,
                       records=[Rec { production=Prod {lhs=Begin, rhs=[(NonTerm "R" []), RightTerm]},
                                      dot=2, pointer=0, lookahead=RightTerm }] }],
        grammar=nseg,
        input=[(Token "a"), (Token "b")],
        currset=3,
        currrec=0
    }

predict_aeg =
    Parser {
        ss=[StateSet { si=0, x=Begin,
                       records=[ Rec { production=Prod {lhs=Begin, rhs=[(NonTerm "E" []), RightTerm]},
                                       dot=0, pointer=0, lookahead=RightTerm }
                                ,Rec { production=Prod {lhs=(NonTerm "E" []), rhs=[(NonTerm "E" []), (Term "+"), (NonTerm "T" [])]},
                                       dot=0, pointer=0, lookahead=RightTerm }
                                ,Rec { production=Prod {lhs=(NonTerm "E" []), rhs=[(NonTerm "T" [])]},
                                       dot=0, pointer=0, lookahead=RightTerm } ] }],
        grammar=aeg,
        input=[(Token "a"), (Token "b")],
        currset=0,
        currrec=0
    }

predict_next_aeg =
    Parser {
        ss=[StateSet { si=0, x=Begin,
                       records=[ Rec { production=Prod {lhs=Begin, rhs=[(NonTerm "E" []), RightTerm]},
                                       dot=0, pointer=0, lookahead=RightTerm }
                                ,Rec { production=Prod {lhs=(NonTerm "E" []), rhs=[(NonTerm "E" []), (Term "+"), (NonTerm "T" [])]},
                                       dot=0, pointer=0, lookahead=RightTerm }
                                ,Rec { production=Prod {lhs=(NonTerm "E" []), rhs=[(NonTerm "T" [])]},
                                       dot=0, pointer=0, lookahead=RightTerm } ] }],
        grammar=aeg,
        input=[(Token "a"), (Token "b")],
        currset=0,
        currrec=1
    }

predict_add0_aeg =
    Parser {
        ss=[StateSet { si=0, x=Begin,
                       records=[ Rec { production=Prod {lhs=Begin, rhs=[(NonTerm "E" []), RightTerm]},
                                       dot=0, pointer=0, lookahead=RightTerm }
                                ,Rec { production=Prod {lhs=(NonTerm "E" []), rhs=[(NonTerm "E" []), (Term "+"), (NonTerm "T" [])]},
                                       dot=0, pointer=0, lookahead=RightTerm }
                                ,Rec { production=Prod {lhs=(NonTerm "E" []), rhs=[(NonTerm "T" [])]},
                                       dot=0, pointer=0, lookahead=RightTerm }
                                ,rec_new_aeg ] }],
        grammar=aeg,
        input=[(Token "a"), (Token "b")],
        currset=0,
        currrec=0
    }

predict_add1_aeg =
    Parser {
        ss=[StateSet { si=0, x=Begin,
                       records=[ Rec { production=Prod {lhs=Begin, rhs=[(NonTerm "E" []), RightTerm]},
                                       dot=0, pointer=0, lookahead=RightTerm }
                                ,Rec { production=Prod {lhs=(NonTerm "E" []), rhs=[(NonTerm "E" []), (Term "+"), (NonTerm "T" [])]},
                                       dot=0, pointer=0, lookahead=RightTerm }
                                ,Rec { production=Prod {lhs=(NonTerm "E" []), rhs=[(NonTerm "T" [])]},
                                       dot=0, pointer=0, lookahead=RightTerm } ] },
            StateSet {si=1, x=(Term "a"), records=[rec_existing_aeg]}
            ],
        grammar=aeg,
        input=[(Token "a"), (Token "b")],
        currset=0,
        currrec=0
    }

predict_nseg =
    Parser {
        ss=[StateSet { si=0, x=Begin,
                       records=[ Rec { production=Prod {lhs=Begin, rhs=[(NonTerm "R" []), RightTerm]},
                                      dot=0, pointer=0, lookahead=RightTerm }
                                ,Rec { production=Prod {lhs=(NonTerm "R" []), rhs=[(NonTerm "A" []), (NonTerm "B" [])]},
                                               dot=0, pointer=0, lookahead=RightTerm } ] }],
        grammar=nseg,
        input=[(Token "b"), (Token "a")],
        currset=0,
        currrec=0
    }

predict_next_nseg =
    Parser {
        ss=[StateSet { si=0, x=Begin,
                       records=[ Rec { production=Prod {lhs=Begin, rhs=[(NonTerm "R" []), RightTerm]},
                                      dot=0, pointer=0, lookahead=RightTerm }
                                ,Rec { production=Prod {lhs=(NonTerm "R" []), rhs=[(NonTerm "A" []), (NonTerm "B" [])]},
                                               dot=0, pointer=0, lookahead=RightTerm } ] }],
        grammar=nseg,
        input=[(Token "b"), (Token "a")],
        currset=0,
        currrec=1
    }

predict_add0_nseg =
    Parser {
        ss=[StateSet { si=0, x=Begin,
                       records=[ Rec { production=Prod {lhs=Begin, rhs=[(NonTerm "R" []), RightTerm]},
                                      dot=0, pointer=0, lookahead=RightTerm }
                                ,Rec { production=Prod {lhs=(NonTerm "R" []), rhs=[(NonTerm "A" []), (NonTerm "B" [])]},
                                               dot=0, pointer=0, lookahead=RightTerm }
                                ,rec_new_nseg ] }],
        grammar=nseg,
        input=[(Token "b"), (Token "a")],
        currset=0,
        currrec=0
    }

predict_add1_nseg =
    Parser {
        ss=[StateSet { si=0, x=Begin,
                       records=[ Rec { production=Prod {lhs=Begin, rhs=[(NonTerm "R" []), RightTerm]},
                                      dot=0, pointer=0, lookahead=RightTerm }
                                ,Rec { production=Prod {lhs=(NonTerm "R" []), rhs=[(NonTerm "A" []), (NonTerm "B" [])]},
                                               dot=0, pointer=0, lookahead=RightTerm } ] },
            StateSet { si=1, x=(Term "b"),
                       records = [rec_existing_nseg]} ],
        grammar=nseg,
        input=[(Token "b"), (Token "a")],
        currset=0,
        currrec=0
    }

complete_before_aeg =
    Parser {
        ss=[StateSet { si=0, x=Begin,
                       records=[ Rec { production=Prod {lhs=Begin, rhs=[(NonTerm "E" []), RightTerm]},
                                       dot=0, pointer=0, lookahead=RightTerm }
                                ,Rec { production=Prod {lhs=(NonTerm "T" []), rhs=[(NonTerm "T" []), (Term "*"), (NonTerm "P" [])]},
                                       dot=2, pointer=0, lookahead=RightTerm }
                                ,Rec { production=Prod {lhs=(NonTerm "T" []), rhs=[(NonTerm "P" [])]},
                                       dot=0, pointer=0, lookahead=(Term "*") } ] },
            StateSet { si=1, x=(Term "a"),
                       records=[ Rec { production=Prod {lhs=(NonTerm "P" []), rhs=[(Term "a")]},
                                       dot=1, pointer=0, lookahead=RightTerm } ] }
            ],
        grammar=aeg,
        input=[(Token "a"), (Token "+")],
        currset=1,
        currrec=0
    }

complete_after_aeg =
    Parser {
        ss=[StateSet { si=0, x=Begin,
                       records=[ Rec { production=Prod {lhs=Begin, rhs=[(NonTerm "E" []), RightTerm]},
                                       dot=0, pointer=0, lookahead=RightTerm }
                                ,Rec { production=Prod {lhs=(NonTerm "T" []), rhs=[(NonTerm "T" []), (Term "*"), (NonTerm "P" [])]},
                                       dot=2, pointer=0, lookahead=RightTerm }
                                ,Rec { production=Prod {lhs=(NonTerm "T" []), rhs=[(NonTerm "P" [])]},
                                       dot=0, pointer=0, lookahead=(Term "*") } ] },
            StateSet { si=1, x=(Term "a"),
                       records=[ Rec { production=Prod {lhs=(NonTerm "P" []), rhs=[(Term "a")]},
                                      dot=1, pointer=0, lookahead=RightTerm }
                                ,Rec { production=Prod {lhs=(NonTerm "T" []), rhs=[(NonTerm "P" [])]},
                                       dot=1, pointer=0, lookahead=(Term "*") }
                                ,Rec { production=Prod {lhs=(NonTerm "T" []), rhs=[(NonTerm "T" []), (Term "*"), (NonTerm "P" [])]},
                                       dot=3, pointer=0, lookahead=RightTerm } ] }
            ],
        grammar=aeg,
        input=[(Token "a"), (Token "+")],
        currset=1,
        currrec=0
    }

scan_before_aeg =
    Parser {
        ss=[StateSet { si=0, x=Begin,
                       records=[ Rec { production=Prod {lhs=Begin, rhs=[(NonTerm "E" []), RightTerm]},
                                       dot=0, pointer=0, lookahead=RightTerm }
                                ,Rec { production=Prod {lhs=(NonTerm "P" []), rhs=[(Term "a")]},
                                       dot=0, pointer=0, lookahead=RightTerm } ] }
            ],
        grammar=aeg,
        input=[(Token "a")],
        currset=0,
        currrec=1
    }

scan_after_aeg =
    Parser {
        ss=[ StateSet { si=0, x=Begin,
                       records=[ Rec { production=Prod {lhs=Begin, rhs=[(NonTerm "E" []), RightTerm]},
                                       dot=0, pointer=0, lookahead=RightTerm }
                                ,Rec { production=Prod {lhs=(NonTerm "P" []), rhs=[(Term "a")]},
                                       dot=0, pointer=0, lookahead=RightTerm } ] }
            ,StateSet { si=1, x=(Term "a"),
                        records=[ Rec { production=Prod {lhs=(NonTerm "P" []), rhs=[(Term "a")]},
                                        dot=1, pointer=0, lookahead=RightTerm } ] }
            ],
        grammar=aeg,
        input=[(Token "a")],
        currset=0,
        currrec=1
    }

scan_nomatch_aeg =
    Parser {
        ss=[ StateSet { si=0, x=Begin,
                       records=[ Rec { production=Prod {lhs=Begin, rhs=[(NonTerm "E" []), RightTerm]},
                                       dot=0, pointer=0, lookahead=RightTerm }
                                ,Rec { production=Prod {lhs=(NonTerm "P" []), rhs=[(Term "a")]},
                                       dot=0, pointer=0, lookahead=RightTerm } ] }
            ],
        grammar=aeg,
        input=[(Token "+")],
        currset=0,
        currrec=1
    }
