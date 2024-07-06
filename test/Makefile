
PWD = $(shell pwd)
harmony ?= $(shell realpath "${PWD}/../harmony")

.PHONY: test clean

INTERACTIVE ?= "--interactive"

export harmony
test:
	idris2 --build harmony-tests.ipkg && \
	./build/exec/test runtests $(INTERACTIVE)

clean:
	rm -rf ./test/build
