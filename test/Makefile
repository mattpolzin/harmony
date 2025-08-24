
PWD = $(shell pwd)
harmony ?= $(shell realpath "${PWD}/../harmony")

.PHONY: test clean

INTERACTIVE ?= "--interactive"

IDR_SOURCES := $(wildcard *.idr)

build/exec/test: $(IDR_SOURCES)
	idris2 --build harmony-tests.ipkg

export harmony
test: build/exec/test
	build/exec/test runtests $(INTERACTIVE) --only $(only)

clean:
	rm -rf ./build
	rm -rf ./graph/build
