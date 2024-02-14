ACMEFLAGS	= -v0
CPUS	:= $(subst .a,,$(subst test-,,$(wildcard test-*.a)))
FILES	= $(foreach N,$(CPUS),$(N).o)

.SILENT:

%.o: test-%.a
	echo "Testing CPU:" $(subst .o,,$@)
	acme $(ACMEFLAGS) -o test.o $<
	cmp test.o expected-$@
	rm test.o
	echo "Ok."

all: $(FILES)
	echo
	echo "Testing CPUs: PASSED"
	echo
