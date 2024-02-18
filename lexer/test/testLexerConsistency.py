#! /bin/python3
# Test the consistency of the lexer since parser needs modified lexer to work with ml-yacc

def read_file(file_path):
    with open(file_path, 'r') as file:
        return file.readlines()

def compare_files(file1, file2):
    lines1 = read_file(file1)
    lines2 = read_file(file2)

    exceptions1 = [
        "type lexresult = Tokens.token"
    ]

    exceptions2 = [
        "type svalue = Tokens.svalue",
        "type ('a,'b) token = ('a,'b) Tokens.token",
        "type lexresult = (svalue,pos) token",
        "%header (functor TigerLexFun(structure Tokens: Tiger_TOKENS));"
    ]

    # drop line in exceptions1 from lines1
    for exception in exceptions1:
        lines1 = [line for line in lines1 if exception not in line]

    for exception in exceptions2:
        lines2 = [line for line in lines2 if exception not in line]

    for line1, line2 in zip(lines1, lines2):
        if line1 != line2:
            print(f"Difference found:\n{line1}vs\n{line2}")
            raise Exception("Lexer files are not consistent, please make sure to modify the parser/src/tiger.lex to match the lexer/src/tiger.lex. Otherwise lexer test results can not reflect the actual lexer behavior.")

if __name__ == "__main__":
    compare_files('lexer/src/tiger.lex', 'parser/src/tiger.lex')