if [ $# -ne 1 ]; then
    echo "run parser tests in the given directory"
    echo "Usage: $0 <dir>"
    exit 1
fi
if [ ! -d $1 ]; then
    echo "Directory $1 does not exist"
    exit 1
fi
echo -n $1 | sml "run-test.sml"