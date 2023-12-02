
idris2 = idris2
idris-adds-version = 0.3.0

idris2-version = $(shell $(idris2) --version | sed -En 's/Idris 2, version ([^-]+).*/\1/p')
idris2-build   = $(shell $(idris2) --version | sed -En 's/Idris 2, version [^-]+(.*)/\1/p')
idris2-minor-version = $(shell echo ${idris2-version} | sed -En 's/0\.(.*)\../\1/p')

.PHONY: all build nix-build install package publish clean version

all: build

depends/idris-adds-${idris-adds-version}:
	mkdir -p depends/idris-adds-${idris-adds-version}
	mkdir -p build/deps
ifeq ($(IDRIS_ADDS_SRC),)
	cd build/deps && \
	  git clone https://github.com/mattpolzin/idris-adds.git && \
	  cd idris-adds && \
	    git checkout ${idris-adds-version} && \
	    make && \
	    cp -R ./build/ttc/* ../../../depends/idris-adds-${idris-adds-version}/
else
	cd build/deps && \
	  cp -R $(IDRIS_ADDS_SRC) ./idris-adds && \
		chmod -R +rw ./idris-adds && \
		cd idris-adds && \
			make && \
	    cp -R ./build/ttc/* ../../../depends/idris-adds-${idris-adds-version}/
endif

./node_modules/: package.json
	npm install

build: ./node_modules/ depends/idris-adds-${idris-adds-version}
	IDRIS2_DATA=./support $(idris2) --build harmony.ipkg
	@if [[ ${idris2-minor-version} -gt 6 ]] || [[ "${idris2-build}" != '' ]]; then \
	  cp ./build/exec/harmony ./harmony; \
	else \
	  echo "#!/usr/bin/env node" > ./harmony; \
	  cat ./build/exec/harmony >> ./harmony; \
	fi
	@chmod +x ./harmony

harmony: build

node2nix ?= nix run nixpkgs\#node2nix

nix-build:
	${MAKE} clean
	$(node2nix) -- --composition node2nix.nix # -l # <- can't use -l for lockfile because lockfile version 3 not supported yet.
	nix build .

version:
	@(if [[ "${v}" == '' ]]; then echo "please set the 'v' variable."; exit 1; fi)
	sed -I '' "s/version = .*/version = ${v}/" ./harmony.ipkg
	sed -I '' "s/appVersion = \".*\"/appVersion = \"${v}\"/" ./src/AppVersion.idr
	sed -I '' "s/\"version\": \".*\"/\"version\": \"${v}\"/" ./package.json

package: build
	./version-check.sh
	./todo-check.sh
	# leave ./harmony in place
	rm -rf ./harmony-npm
	mkdir harmony-npm
	cp harmony ./harmony-npm/
	cp package.json ./harmony-npm/
	cp README.md ./harmony-npm/
	tar -czvf harmony-npm.tar.gz harmony-npm

publish : package
	npm publish ./harmony-npm

install: harmony
	npm install --global

clean:
	rm -rf ./depends
	rm -rf ./build
	rm -rf ./node_modules
	rm -rf ./harmony-npm
	rm -f ./harmony
	rm -f ./harmony-npm.tar.gz
