
idris2 ?= idris2
idris2-version = $(shell $(idris2) --version | sed -En 's/Idris 2, version ([^-]+).*/\1/p')
idris2-build   = $(shell $(idris2) --version | sed -En 's/Idris 2, version [^-]+(.*)/\1/p')
idris2-minor-version = $(shell echo ${idris2-version} | sed -En 's/0\.(.*)\../\1/p')

idris-adds-version = 0.4.1
idris-json-version = 0.5.0
idris-json-hash    = 2e54a37ed3c35c2d12c8927c923ad253355812a8
idris-elab-util-version = 0.6.0
ifeq ($(idris2-version)$(idris2-build),0.7.0)
  idris-elab-util-hash    = 2fc2d188640ce6822b5e250db73b62f5a952ca4d
else
  # breaking change between versions 0.7.0 and 0.8.0 of Idris2:
  idris-elab-util-hash    = dc875d4a8a2aa3bf0a767fb586a4d9150d83363d
endif
idris-parser-version = 0.1.0
idris-parser-hash    = 0fde36cf11c12a61edcfe09d585c5a60426bc706

kernel = $(shell uname -s)

ifeq ($(kernel),Linux)
	ised = sed -i''
else
	ised = sed -I ''
endif

.PHONY: all build test nix-build install package publish clean version

all: build

depends/idris-adds-${idris-adds-version}:
	mkdir -p depends/idris-adds-${idris-adds-version}
	mkdir -p build/deps
	cd build/deps && \
		if [ ! -d ./idris-adds ]; then \
			git clone https://github.com/mattpolzin/idris-adds.git; \
		fi && \
	  cd idris-adds && \
	    git checkout ${idris-adds-version} && \
	    make && \
	    cp -R ./build/ttc/* ../../../depends/idris-adds-${idris-adds-version}/

depends/elab-util-${idris-elab-util-version}:
	mkdir -p depends/elab-util-${idris-elab-util-version}
	mkdir -p build/deps
	cd build/deps && \
		if [ ! -d ./idris2-elab-util ]; then \
			git clone https://github.com/stefan-hoeck/idris2-elab-util.git; \
		fi && \
	  cd idris2-elab-util && \
	    git checkout ${idris-elab-util-hash} && \
	    $(idris2) --build elab-util.ipkg && \
	    cp -R ./build/ttc/* ../../../depends/elab-util-${idris-elab-util-version}/

depends/parser-${idris-parser-version}: depends/elab-util-${idris-elab-util-version}
	mkdir -p depends/parser-${idris-parser-version}
	mkdir -p build/deps
	cd build/deps && \
		if [ ! -d ./idris2-parser ]; then \
			git clone https://github.com/stefan-hoeck/idris2-parser.git; \
		fi && \
	  cd idris2-parser && \
	    git checkout ${idris-parser-hash} && \
			IDRIS2_PACKAGE_PATH="$IDRIS2_PACKAGE_PATH:../../../depends" $(idris2) --build parser.ipkg && \
	    cp -R ./build/ttc/* ../../../depends/parser-${idris-parser-version}/

define PATCH
5c5,6
< depends   = parser
---
> depends   = elab-util
>           , parser
endef

export PATCH
depends/parser-json-${idris-parser-version}: depends/parser-${idris-parser-version}
	mkdir -p depends/parser-json-${idris-parser-version}
	mkdir -p build/deps
	cd build/deps && \
		if [ ! -d ./idris2-parser ]; then \
			git clone https://github.com/stefan-hoeck/idris2-parser.git; \
		fi && \
	  cd idris2-parser/json && \
	    git checkout ${idris-parser-hash} && \
			echo "$$PATCH" | patch parser-json.ipkg - && \
			IDRIS2_PACKAGE_PATH="$IDRIS2_PACKAGE_PATH:../../../../depends" $(idris2) --build parser-json.ipkg && \
	    cp -R ./build/ttc/* ../../../../depends/parser-json-${idris-parser-version}/

depends/json-${idris-json-version}: depends/elab-util-${idris-elab-util-version} depends/parser-${idris-parser-version} depends/parser-json-${idris-parser-version}
	mkdir -p depends/json-${idris-json-version}
	mkdir -p build/deps
	cd build/deps && \
		if [ ! -d ./idris2-json ]; then \
			git clone https://github.com/stefan-hoeck/idris2-json.git; \
		fi && \
	  cd idris2-json && \
	    git checkout ${idris-json-hash} && \
			IDRIS2_PACKAGE_PATH="$IDRIS2_PACKAGE_PATH:../../../depends" $(idris2) --build json.ipkg && \
	    cp -R ./build/ttc/* ../../../depends/json-${idris-json-version}/

./node_modules/: package.json
	npm install

build: ./node_modules/ depends/idris-adds-${idris-adds-version} depends/json-${idris-json-version}
	IDRIS2_DATA=./support $(idris2) --build harmony.ipkg
	@if [ ${idris2-minor-version} -gt 6 ] || [ "${idris2-build}" != '' ]; then \
	  cp ./build/exec/harmony ./harmony; \
	else \
	  echo "#!/usr/bin/env node" > ./harmony; \
	  cat ./build/exec/harmony >> ./harmony; \
	fi
	@chmod +x ./harmony

harmony: build

nix ?= nix

# This executes a Nix build. Call as `make nix-build` from CLI, not
# from a Nix file.
nix-build:
	${MAKE} clean
	$(nix) build .

version:
	@(if [[ "${v}" == '' ]]; then echo "please set the 'v' variable."; exit 1; fi)
	$(ised) "s/version = .*/version = ${v}/" ./harmony.ipkg
	$(ised) "s/appVersion = \".*\"/appVersion = \"${v}\"/" ./src/AppVersion.idr
	$(ised) "s/\"version\": \".*\"/\"version\": \"${v}\"/" ./package.json
	@npm update
	@$(nix) fmt

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

test:
	$(MAKE) -C test test

clean:
	rm -rf ./depends
	rm -rf ./build
	rm -rf ./node_modules
	rm -rf ./harmony-npm
	rm -f ./harmony
	rm -f ./harmony-npm.tar.gz
	$(MAKE) -C test clean
