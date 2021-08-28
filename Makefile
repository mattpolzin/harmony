
idris2 = idris2

.PHONY: all build clean

all: build

build:
	IDRIS2_DATA=./support $(idris2) --build harmony.ipkg
	cp ./build/exec/harmony ./harmony.js

clean:
	rm -rf ./build/
	rm harmony.js

