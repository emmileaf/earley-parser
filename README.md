# Earley Parser


This project is an exercise to implement (in Haskell) the Earley parsing algorithm along with two example grammars as described in the following paper:

```
Earley, J. (1970). An efficient context-free parsing algorithm. Communications of the ACM, 13(2), 94-102.
https://dl.acm.org/doi/10.1145/362007.362035
```

The program can be built and executed with the following command:

```
$ stack run
```
Alternatively, individual modules and functions can be loaded and called via GHCi as follows:
```
$ stack ghci
> :l Main
> main
```
Once running, the REPL will show the following prompt:
```
Hello! Please specify a grammar out of the following: [AE, NSE]
>
```

You can specify one of these grammars, followed by an input string to be parsed. for example:

```
Hello! Please specify a grammar out of the following: [AE, NSE]
> AE
Please enter an input to be parsed
> a+a*a
"( S: (E (E (T (P a))) + (T (T (P a)) * (P a))) -|)"
Please enter an input to be parsed
> a+a
"( S: (E (E (T (P a))) + (T (P a))) -|)"
Please enter an input to be parsed
> quit
GoodBye!
```

```
Hello! Please specify a grammar out of the following: [AE, NSE]
> NSE
Please enter an input to be parsed
> ab
"( S: (R (A a) (B b)) -|)"
Please enter an input to be parsed
> abcdb
"( S: (R (A (R (A a) (B b)) (C c)) (B (D d) (B b))) -|)"
Please enter an input to be parsed
> quit
GoodBye!
```

### Code Components

```
earley
├── app
│   ├── Main.hs
├── src
│   ├── Grammar
│   │   ├── Core.hs
│   │   ├── GrammarAE.hs
│   │   └── GrammarNSE.hs
│   ├── Parser
│   │   ├── Core.hs
│   │   └── Earley.hs
├── test
│   ├── Mocks.hs
│   ├── Spec.hs
│   └── Tests.hs
├── package.yaml
├── stack.yaml
└── README.md
```

1. The `src` directory contains representations of:
    - Two example grammars (with data types and functions defined in `Grammar`)
    - A parser running the Earley algorithm (with data types and functions defined in `Parser`).  

2. The `app` directory contains the main executable, which provides a REPL to interact with the parser.  

3. The `test` directory contains the test suite of unit, feature, and integration tests.

Due to the scope limitations of this project, some aspects of the algorithm were not included in this implementation:
1.  The completer adds records to the current set regardless of the source records's lookahead string. While this does not affect the correctness of the algorithm, the extra condition for lookahead matching is described in the paper and improves
the algorithm's efficiency.  

2. Grammars with null productions are not supported or tested by this implementation.

3. Tokenization of input strings is breaks them into single characters only. Adding
a grammar with symbols represented by more than one character cannot be done without also updating the current method of tokenization.



The `test` directory contains the test suite of unit, feature, and integration tests. The test-suite can be run with with the following command:

```
$ stack test
```

These ensure that all pieces of the implementation behave as expected, and
provide a measure of program execution time.

```
[TEST] Tests:parser_aeg (test/Tests.hs:20)
+++ OK (0ms)

(... output omitted ...)

* Tests:     15
* Passed:    15
* Pending:   0
* Failures:  0
* Errors:    0
* Timed out: 0
* Filtered:  0

Total execution time: 1ms
```
