#!/bin/bash
# call the semantic analysis program on all files in the given directory

if [ $# -eq 0 ]; then
    echo "run semantic tests in the given directory"
    echo "if received a directory, run all tests in the directory"
    echo "if received files, run tests for the files"
    echo "Usage: $0 <dir> | <file1> <file2> ..."
    exit 1
fi

if [ -d $1 ]; then
    mkdir -p ../resources/tmp
    for file in $1/*.tig; do
        filename=$(basename $file)
        echo -n $file | sml "test.sml" 2>&1 > ../resources/tmp/${filename}_out.txt
        # compare the output with the truth, if different, print the difference and the file name and exit
    done
    echo "All tests finished"
    exit 0
fi

echo -n $1 | sml "test.sml"
echo "Test finished"
exit 0

