
idris2 = idris2

.PHONY: all build clean

all: build

node_modules:
	npm install

build: node_modules
	IDRIS2_DATA=./support $(idris2) --build harmony.ipkg
	cp ./build/exec/harmony ./harmony.js

clean:
	rm -rf ./build/
	rm -rf ./node_modules
	rm harmony.js

