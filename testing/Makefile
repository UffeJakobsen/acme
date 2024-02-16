
.SILENT:

.PHONY: auto bugfixes cpus errors warnings

all: auto bugfixes cpus errors warnings

auto:
	echo "Performing self-tests..."
	echo
	make -C auto

bugfixes:
	echo "Testing bugfixes..."
	echo
	make -C bugfixes

cpus:
	echo "Testing CPUs..."
	echo
	make -C cpus

errors:
	echo "Testing errors..."
	echo
	make -C errors

warnings:
	echo "Testing warnings..."
	echo
	make -C warnings
