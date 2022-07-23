
idris2 = idris2
idris-adds-version = 0.3.0

.PHONY: all build install package publish clean

all: build

depends/idris-adds-${idris-adds-version}:
	mkdir -p depends/idris-adds-${idris-adds-version}
	mkdir -p build/deps
	cd build/deps && \
	  git clone https://github.com/mattpolzin/idris-adds.git && \
	  cd idris-adds && \
	    git checkout ${idris-adds-version} && \
	    make && \
	    cp -R ./build/ttc/* ../../../depends/idris-adds-${idris-adds-version}/

node_modules:
	npm install

build: node_modules depends/idris-adds-${idris-adds-version}
	IDRIS2_DATA=./support $(idris2) --build harmony.ipkg
	@echo "#!/usr/bin/env node\n" > ./harmony
	@cat ./build/exec/harmony >> ./harmony
	@chmod +x ./harmony

harmony: build

package: build
	./version-check.sh
	./todo-check.sh
	# leave ./harmony in place
	mkdir harmony-npm
	cp harmony ./harmony-npm/
	cp package.json ./harmony-npm/
	cp README.md ./harmony-npm/
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

