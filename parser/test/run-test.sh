#!/bin/bash
if [ $# -eq 0 ]; then
    echo "run parser tests in the given directory"
    echo "if received a directory, run all tests in the directory"
    ehco "if received files, run tests for the files"
    echo "Usage: $0 <dir> | <file1> <file2> ..."
    exit 1
fi
mkdir -p ../resources/tmp
# if the first argument is a directory
if [ -d $1 ]; then
    for file in $1/*.tig; do
        # redirect standard error to ../resources/truth/{filename}_truth.txt
        # get the base of the file name
        filename=$(basename $file)
        truthfile=../resources/truth/${filename}_truth.txt
        outfile=../resources/tmp/${filename}_out.txt
        # redirect stdout to dev/null
        echo -n $file | sml "run-test.sml" 2> ../resources/tmp/${filename}_out.txt > /dev/null
        # compare the output with the truth, if different, print the difference and the file name and exit
        diff $truthfile $outfile
        if [ $? -ne 0 ]; then
            echo "Test failed: $file"
            exit 1
        fi
    done
    echo "All tests passed"
    exit 0
fi

# if the first argument is not a directory
# run tests for the files
for file in $@; do
    filename=$(basename $file)
    truthfile=../resources/truth/${filename}_truth.txt
    outfile=../resources/tmp/${filename}_out.txt
    echo -n $file | sml "run-test.sml" 2> ../resources/tmp/${filename}_out.txt > /dev/null
    diff $truthfile $outfile
    if [ $? -ne 0 ]; then
        echo "Test failed: $file"
        exit 1
    fi
done
echo "All tests passed"
exit 0
