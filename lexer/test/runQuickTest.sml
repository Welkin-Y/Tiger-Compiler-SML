#!/bin/bash
usage() {
	echo "Usage: $0 [path/to/simpleTest.sml]"
	echo "If filelist.txt is not provided, a default one will be created from files under lexer/resources/"
}
		
if [ $# -eq 0 ]; then
	echo "Use default one will be created from files under lexer/resources/"
	find lexer/resources/ -type f > filelist.tmp.txt
else 
	if [ $# -eq 1]; then
		cp "$1" filelist.tmp.txt

	else
		usage
		exit 1
	fi
fi
cat lexer/test/simpleTest.sml | sml
rm filelist.tmp.txt
