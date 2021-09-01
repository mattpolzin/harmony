
idris2 = idris2

.PHONY: all build clean

all: build

node_modules:
	npm install

build: node_modules
	IDRIS2_DATA=./support $(idris2) --build harmony.ipkg
	@echo "#!/usr/bin/env node\n" > ./harmony
	@cat ./build/exec/harmony >> ./harmony
	@chmod +x ./harmony

clean:
	rm -rf ./build/
	rm -rf ./node_modules
	rm ./harmony

