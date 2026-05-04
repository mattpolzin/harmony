
pack ?= pack
idris2 ?= $(shell $(pack) app-path idris2)
pack_package_path = $(shell pack package-path)
IDRIS2_PACKAGE_PATH := "$(pack_package_path):${IDRIS2_PACKAGE_PATH}"

PWD = $(shell pwd)
harmony ?= $(shell realpath "${PWD}/../harmony")

.PHONY: deps test clean

INTERACTIVE ?= "--interactive"

IDR_SOURCES := $(wildcard *.idr)

deps:
	$(pack) install-deps

build/exec/test: deps $(IDR_SOURCES)
	$(idris2) --build harmony-tests.ipkg

export harmony
export idris2
export IDRIS2_PACKAGE_PATH
test: build/exec/test
	build/exec/harmony-test runtests $(INTERACTIVE) --only "$(only)" --except "$(except)"

clean:
	rm -rf ./build
