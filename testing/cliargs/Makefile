#ACMEFLAGS	= -v0

.PHONY: outformats

.SILENT:

all: outformats
	echo
	echo "Testing cliargs: PASSED"
	echo

outformats: outformats.a outformats.sh outformat-*.o
	echo "Testing output formats:"
	bash outformats.sh
