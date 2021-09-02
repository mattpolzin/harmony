
idris2 = idris2

.PHONY: all build install clean

all: build

node_modules:
	npm install

build: node_modules
	IDRIS2_DATA=./support $(idris2) --build harmony.ipkg
	@echo "#!/usr/bin/env node\n" > ./harmony
	@cat ./build/exec/harmony >> ./harmony
	@chmod +x ./harmony

harmony: build

install: harmony
	npm install --global

clean:
	rm -rf ./build/
	rm -rf ./node_modules
	rm ./harmony

