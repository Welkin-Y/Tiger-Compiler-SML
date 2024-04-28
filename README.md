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
SML version:    Standard ML of New Jersey (64-bit) v110.99.5
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
│   ├── negative
│   └── positive
├── frame_ir
│   ├── src
│   └── test
├── lexer
│   └── src
├── logger
│   └── src
├── parser
│   ├── resources
│   │   └── tmp
│   ├── src
│   └── test
├── semantic
│   ├── resources
│   │   └── tmp
│   ├── src
│   └── test
├── src
│   ├── absyn
│   ├── env
│   ├── graph
│   ├── insel
│   ├── ir
│   ├── lexer
│   │   └── deprecated
│   ├── liveness
│   ├── logger
│   ├── mips
│   ├── parser
│   ├── regalloc
│   ├── semantic
│   ├── table
│   └── tree
└── test
    ├── insel
    │   └── examples
    ├── ir
    ├── lexer
    │   └── resources
    ├── live_reg
    ├── logger
    ├── parser
    │   └── resources
    │       └── truth
    └── sematic
        └── resources
            ├── negative
            └── positive

```


## Running the Code
### lexer

We've developed test scripts to run all predefined tests under the `lexer/resources` directory.

To do this, follow these steps:

1. Navigate to `test/lexer`.
2. Run the `runTest.sh` script:

```
>> cd /path/to/test/lexer-parent/
>> ./runTest.sh
```

You can also directly call our lexer for a specific test file using the following SML code:

```
>> CM.make "lexer/src/resources.cm";
>> Parse.parse "your-test-source-file";
```
### parser
We've developed test scripts to run all predefined tests under the `test/parser/resources` directory.


To do this, follow these steps:

1. Navigate to the parent directory of `test/parser`.
2. Run the `runTest.sh` script:

```
cd /path/to/parser-parent/parser/test
./runTest.sh ./resources
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


## IR Translation
**note**: We uses **1 late days** for this phase. Also we noticed that we forgot to mention the bonus of the Semantic Analysis in the previous phase. The implemnted bonus list can be found in the next section. Request for regrading is appreciated.

We implemented our semantic analysis body in `src/translate.sml` and tested it with the self-defined and provided test cases. 

The find escape function in the parser phase is used to analyze the variable escaping over the absract syntax tree.

## Instruction Selection
We used the Appel's implementation of `canon.sml` and implemented our version of mipsgen in `mipsgen.sml`. Some bugs from the previous phases are fixed in this phase. The `Semantic.sml` is also updated to fix bugs in the previous phase.

We also reorganized the code structure to make it more readable and easier to understand.

## Liveness Analysis & Register Allocation
We implemented the liveness analysis and register allocation in `src/liveness/` and `src/regalloc`. The liveness analysis is based on the Appel's instruction level node for control flow graph. The register allocation is based on the graph coloring algorithm. 
Some bugs from the previous phases are fixed in this phase like view shift. 
To run our code:
```
cd test/live_reg
echo exmaple.tig | sml test.sml
```
which will generate an `example.tig.s` file in the same directory contains the assembly code.


## Putting it all together
**note** We used **3 late days** for this phase.
We fixed multiple bugs in the previous phases and developed a script to add runtime library to the generated assembly code. 
You can run example of the code by:
```
cd test/live_reg
sml drew.sml
bash spim.sh
```
`bash spim.sh` will compile the `example.tig` in the directory and generate a `example.s` file. You can run it using the spim simulator.


## Implemented Bonus
- **Semantic Analysis**: We implemented drew's version of Record. We use the functional record to achive the recursive definition
- **IR Translation**: We implemented the drew's version of the Tree data structures according to the lecture to differentiate loc from exp. Increase the robustness of the code and make it easier to understand.