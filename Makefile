
idris2 = idris2

.PHONY: all build install package publish clean

all: build

depends/idris-adds-0.1.0:
	mkdir -p depends/idris-adds-0.1.0
	mkdir -p build/deps
	cd build/deps && \
	  git clone https://github.com/mattpolzin/idris-adds.git && \
	  cd idris-adds && \
	    git checkout 0.1.0 && \
	    make && \
	    cp -R ./build/ttc/* ../../../depends/idris-adds-0.1.0/

node_modules:
	npm install

build: node_modules depends/idris-adds-0.1.0
	IDRIS2_DATA=./support $(idris2) --build harmony.ipkg
	@echo "#!/usr/bin/env node\n" > ./harmony
	@cat ./build/exec/harmony >> ./harmony
	@chmod +x ./harmony

harmony: build

package: build
	./version-check.sh
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
	rm -rf ./depends
	rm -rf ./build
	rm -rf ./node_modules
	rm -f ./harmony
	rm -f ./harmony-npm.tar.gz

