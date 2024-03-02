# Tiger Compiler in SML - Documentation

[![License](https://img.shields.io/badge/License-GLPv3-blue.svg)](https://gitlab.oit.duke.edu/zy96/553-compiler/-/blob/main/LICENSE)
[![Build Status](https://gitlab.oit.duke.edu/zy96/553-compiler/badges/main/pipeline.svg)](https://gitlab.oit.duke.edu/zy96/553-compiler/pipelines)

By Frank Fu (qf37), Janus Chen (jc977), and Welkin Yuan (zy96).

## Test Environment
```
Distributor ID: Ubuntu  
Description:    Ubuntu 22.04.3 LTS  
Release:        22.04  
Codename:       jammy  
```

## Dependencies
Standard ML of New Jersey v110.79 [built: Sat Oct 26 12:27:04 2019]  
Python 3.10.12  
```
sudo apt-get update -y  
sudo apt-get install -y smlnj  
sudo apt-get install -y ml-ulex 
sudo apt-get install -y ml-yacc 
sudo apt-get install -y python3 python3-pip  
```

## Code Structure

Our codebase is organized as follows:

```
553-compiler/
├── examples
│   ├── negative
│   └── positive
├── lexer
│   ├── resources
│   ├── src
│   └── test
├── parser
│   ├── resources
│   │   ├── tmp
│   │   └── truth
│   ├── src
│   └── test
└── semantic
    ├── resources
    │   ├── negative
    │   ├── positive
    │   └── tmp
    ├── src
    └── test
```
- `examples` directory: Contains test tiger source code files from textbook
- `resources` directory: Contains test tiger source code files.
- `src` directory: Contains our main work, with the primary component being `src/tiger.lex`. Other files are provided by the textbook. We've also made improvements to `src/errormsg.sml` for better testing.
- `test` directory: Contains code for testing. `simpleTest.sml` is a small test program for running a single test, and `runTest.sh` is a script for customizing test runs.

## Running the Code
### lexer

We've developed test scripts to run all predefined tests under the `lexer/resources` directory.

To do this, follow these steps:

1. Navigate to the parent directory of `lexer`.
2. Run the `runTest.sh` script:

```
>> bashCopy codecd /path/to/lexer-parent/
>> lexer/test/runTest.sh
```

You can also directly call our lexer for a specific test file using the following SML code:

```
>> smlCopy codeCM.make "lexer/src/resources.cm";
>> Parse.parse "your-test-source-file";
```
### parser
We've developed test scripts to run all predefined tests under the `parser/resources` directory.


To do this, follow these steps:

1. Navigate to the parent directory of `parser/test`.
2. Run the `runTest.sh` script:

```
cd /path/to/parser-parent/parser/test
./runTest.sh ../resources
```



## Design Overview
### Lexer

We leveraged ML-Lex to facilitate our DFA lexer implementation.

#### Comments

Our lexer handles comments by "absorbing" them. It also supports nested comments. We introduced a new state called `COMMENT` in ML-Lex. When we encounter `/*`, we transition to the `COMMENT` state. To handle nested comments, we use the `commentDepth` variable in `errormessage.sml` to record the depth of nested comments. We return to the `INITIAL` state only when we encounter `*/` and the `commentDepth` is 0. All content within the `COMMENT` state is ignored.

#### Strings

We defined a `STRING` state to handle special characters in string literals. The lexer enters the `STRING` state when it encounters a quotation mark. The `STRING` state machine deals with multiline string formats using a specially designed regex pattern `/f___f/` and replaces supported escaped characters. The string literals stored in tokens will be represented without escape characters and multiline indicators (`/`).

### Parser
We leveraged ML-Yacc to facilitate our parser implementation.We adapted our lexer to work with ML-Yacc.

All mentioned grammar rules in textbook are implemented and tested. 
#### shift-reduce conflict
Our parser have **0** shift-reduce conflict and 0 reduce-reduce conflict.
We used two methods to resolve the shift-reduce conflict:

1. for shift-reduce conflicts due to precedence, we used the `%nonassoc`, `%left`, `%right` directive to resolve the conflict.

2. for shift-reduce conflicts due to ambiguity like the rules related to `tydec`, `fundec`. We add additional rules to resolve the conflict.



### Semantic Analysis
**note**: We uses **2 late days** for this phase and we fixed the bug in the parser phase. Request for regrading is appreciated.

We implemented our semantic analysis body in `src/semant.sml` and tested it with the self-defined and provided test cases.

We implemented our findescape function in `parser/src/findescape.sml` to analyze the variable escaping over the absract syntax tree.

