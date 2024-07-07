
PWD = $(shell pwd)
harmony ?= $(shell realpath "${PWD}/../harmony")

.PHONY: test clean

INTERACTIVE ?= "--interactive"

IDR_SOURCES := $(wildcard *.idr)

build/exec/test: $(IDR_SOURCES)
	idris2 --build harmony-tests.ipkg

export harmony
test: build/exec/test
	cp config-command/harmony.json harmony.json.bak && \
	  build/exec/test runtests $(INTERACTIVE) && \
	  mv harmony.json.bak config-command/harmony.json

clean:
	rm -rf ./test/build
