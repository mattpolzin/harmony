
idris2 = idris2

.PHONY: all build install package publish clean

all: build

node_modules:
	npm install

build: node_modules
	IDRIS2_DATA=./support $(idris2) --build harmony.ipkg
	@echo "#!/usr/bin/env node\n" > ./harmony
	@cat ./build/exec/harmony >> ./harmony
	@chmod +x ./harmony

harmony: build

package: build
	./version-check.sh
	rm -rf ./build
	rm -rf ./node_modules
	# leave ./harmony in place
	mkdir harmony-npm
	cp harmony ./harmony-npm
	cp package.json ./harmony-npm
	tar -czvf harmony-npm.tar.gz harmony-npm
	rm -rf ./harmony-npm

publish : package
	npm publish harmony-npm.tar.gz

install: harmony
	npm install --global

clean:
	rm -rf ./build
	rm -rf ./node_modules
	rm -f ./harmony
	rm -f ./harmony-npm.tar.gz

