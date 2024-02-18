#!/bin/bash

check() {
	#echo "$@"
	FILE="$1"
	shift
	acme "$@" outformats.a || exit 1
	cmp test.o "$FILE" || exit 1
	rm test.o
}

# if neither -o nor -f are given, use format from "!to", which defaults to cbm:
check outformat-cbm.o	-DFORMAT=0
check outformat-plain.o	-DFORMAT=1
check outformat-cbm.o	-DFORMAT=2
check outformat-apple.o	-DFORMAT=3

# if -o or -f are given, format from "!to" should be ignored:
for f in 0 1 2 3 ; do
	check outformat-plain.o	-DFORMAT=$f -f plain
	check outformat-cbm.o	-DFORMAT=$f -f cbm
	check outformat-apple.o	-DFORMAT=$f -f apple
	check outformat-plain.o	-DFORMAT=$f -o test.o	# defaults to plain
	check outformat-plain.o	-DFORMAT=$f -o test.o -f plain
	check outformat-cbm.o	-DFORMAT=$f -o test.o -f cbm
	check outformat-apple.o	-DFORMAT=$f -o test.o -f apple
done
