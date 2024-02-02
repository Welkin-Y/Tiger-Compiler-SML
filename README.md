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
sudo apt-get install -y python3 python3-pip  
```

## Code Structure

Our codebase is organized as follows:

```
lexer
├── resources 
│   ├── basic.tig
│   ├── basicComment.tig
│   ├── comment_jc977.tig
│   ├── comment_qf.tig
│   ├── id_jc977.tig
│   ├── identifier_qf.tig
│   ├── largeInt_qf.tig
│   ├── leadingZeroInt_qf.tig
│   ├── numbers.tig
│   ├── queens.tig
│   ├── string_qf.tig
│   ├── string_zy.tig
│   └── strings_jc977.tig
├── src
│   ├── driver.sml
│   ├── errormsg.sml
│   ├── resources.cm
│   ├── tiger.lex
│   ├── tiger.lex.sml
│   ├── tokens.sig
│   └── tokens.sml
└── test
    ├── jc977Test.sml
    ├── qfTest.sml
    ├── runTest.sh
    └── simpleTest.sml
```

- `resources` directory: Contains test tiger source code files.
- `src` directory: Contains our main work, with the primary component being `src/tiger.lex`. Other files are provided by the textbook. We've also made improvements to `src/errormsg.sml` for better testing.
- `test` directory: Contains code for testing. `simpleTest.sml` is a small test program for running a single test, and `runTest.sh` is a script for customizing test runs.

## Running the Code

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

## Design Overview

We leveraged ML-Lex to facilitate our DFA lexer implementation.

### Comments

Our lexer handles comments by "absorbing" them. It also supports nested comments. We introduced a new state called `COMMENT` in ML-Lex. When we encounter `/*`, we transition to the `COMMENT` state. To handle nested comments, we use the `commentDepth` variable in `errormessage.sml` to record the depth of nested comments. We return to the `INITIAL` state only when we encounter `*/` and the `commentDepth` is 0. All content within the `COMMENT` state is ignored.

### Strings

We defined a `STRING` state to handle special characters in string literals. The lexer enters the `STRING` state when it encounters a quotation mark. The `STRING` state machine deals with multiline string formats using a specially designed regex pattern `/f___f/` and replaces supported escaped characters. The string literals stored in tokens will be represented without escape characters and multiline indicators (`/`).
