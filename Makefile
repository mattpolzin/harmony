
idris2 = idris2

idris-adds-version = 0.3.0
idris-json-version = 0.5.0
idris-json-hash    = 2e54a37ed3c35c2d12c8927c923ad253355812a8
idris-elab-util-version = 0.6.0
idris-elab-util-hash    = 2fc2d188640ce6822b5e250db73b62f5a952ca4d
idris-parser-version = 0.1.0
idris-parser-hash    = 0fde36cf11c12a61edcfe09d585c5a60426bc706

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
		if [ ! -d ./idris-adds ]; then \
			git clone https://github.com/mattpolzin/idris-adds.git; \
		fi && \
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

depends/elab-util-${idris-elab-util-version}:
	mkdir -p depends/elab-util-${idris-elab-util-version}
	mkdir -p build/deps
ifeq ($(IDRIS_ELAB_UTIL_SRC),)
	cd build/deps && \
		if [ ! -d ./idris2-elab-util ]; then \
			git clone https://github.com/stefan-hoeck/idris2-elab-util.git; \
		fi && \
	  cd idris2-elab-util && \
	    git checkout ${idris-elab-util-hash} && \
	    $(idris2) --build elab-util.ipkg && \
	    cp -R ./build/ttc/* ../../../depends/elab-util-${idris-elab-util-version}/
else
	cd build/deps && \
	  cp -R $(IDRIS_ELAB_UTIL_SRC) ./elab-util && \
		chmod -R +rw ./elab-util && \
		cd elab-util && \
	    $(idris2) --build elab-util.ipkg && \
	    cp -R ./build/ttc/* ../../../depends/elab-util-${idris-elab-util-version}/
endif

depends/parser-${idris-parser-version}: depends/elab-util-${idris-elab-util-version}
	mkdir -p depends/parser-${idris-parser-version}
	mkdir -p build/deps
ifeq ($(IDRIS_PARSER_SRC),)
	cd build/deps && \
		if [ ! -d ./idris2-parser ]; then \
			git clone https://github.com/stefan-hoeck/idris2-parser.git; \
		fi && \
	  cd idris2-parser && \
	    git checkout ${idris-parser-hash} && \
			IDRIS2_PACKAGE_PATH="$IDRIS2_PACKAGE_PATH:../../../depends" $(idris2) --build parser.ipkg && \
	    cp -R ./build/ttc/* ../../../depends/parser-${idris-parser-version}/
else
	cd build/deps && \
	  cp -R $(IDRIS_PARSER_SRC) ./idris2-parser && \
		chmod -R +rw ./idris2-parser && \
		cd idris2-parser && \
			IDRIS2_PACKAGE_PATH="$IDRIS2_PACKAGE_PATH:../../../depends" $(idris2) --build parser.ipkg && \
	    cp -R ./build/ttc/* ../../../depends/parser-${idris-parser-version}/
endif

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
ifeq ($(IDRIS_PARSER_SRC),)
	cd build/deps && \
		if [ ! -d ./idris2-parser ]; then \
			git clone https://github.com/stefan-hoeck/idris2-parser.git; \
		fi && \
	  cd idris2-parser/json && \
	    git checkout ${idris-parser-hash} && \
			echo "$$PATCH" | patch parser-json.ipkg - && \
			IDRIS2_PACKAGE_PATH="$IDRIS2_PACKAGE_PATH:../../../../depends" $(idris2) --build parser-json.ipkg && \
	    cp -R ./build/ttc/* ../../../../depends/parser-json-${idris-parser-version}/
else
	cd build/deps && \
		if [ ! -d ./idris2-parser ]; then \
			cp -R $(IDRIS_PARSER_SRC) ./idris2-parser && \
			chmod -R +rw ./idris2-parser; \
		fi && \
		cd idris2-parser/json && \
			echo "$$PATCH" | patch parser-json.ipkg - && \
			IDRIS2_PACKAGE_PATH="$IDRIS2_PACKAGE_PATH:../../../../depends" $(idris2) --build parser-json.ipkg && \
	    cp -R ./build/ttc/* ../../../../depends/parser-json-${idris-parser-version}/
endif

depends/json-${idris-json-version}: depends/elab-util-${idris-elab-util-version} depends/parser-${idris-parser-version} depends/parser-json-${idris-parser-version}
	mkdir -p depends/json-${idris-json-version}
	mkdir -p build/deps
ifeq ($(IDRIS_JSON_SRC),)
	cd build/deps && \
		if [ ! -d ./idris2-json ]; then \
			git clone https://github.com/stefan-hoeck/idris2-json.git; \
		fi && \
	  cd idris2-json && \
	    git checkout ${idris-json-hash} && \
			IDRIS2_PACKAGE_PATH="$IDRIS2_PACKAGE_PATH:../../../depends" $(idris2) --build json.ipkg && \
	    cp -R ./build/ttc/* ../../../depends/json-${idris-json-version}/
else
	cd build/deps && \
		cp -R $(IDRIS_JSON_SRC) ./idris2-json && \
		chmod -R +rw ./idris2-json && \
		cd idris2-json && \
			IDRIS2_PACKAGE_PATH="$IDRIS2_PACKAGE_PATH:../../../depends" $(idris2) --build json.ipkg && \
	    cp -R ./build/ttc/* ../../../depends/json-${idris-json-version}/
endif

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

node2nix ?= nix run nixpkgs\#node2nix

# This executes a Nix build. Call as `make nix-build` from CLI, not
# from a Nix file.
nix-build:
	${MAKE} clean
	$(node2nix) -- --composition node2nix.nix # -l # <- can't use -l for lockfile because lockfile version 3 not supported yet.
	nix build .

version:
	@(if [[ "${v}" == '' ]]; then echo "please set the 'v' variable."; exit 1; fi)
	sed -I '' "s/version = .*/version = ${v}/" ./harmony.ipkg
	sed -I '' "s/appVersion = \".*\"/appVersion = \"${v}\"/" ./src/AppVersion.idr
	sed -I '' "s/\"version\": \".*\"/\"version\": \"${v}\"/" ./package.json
	sed -I '' "s/version = \".*\";/version = \"${v}\";/" ./default.nix
	@npm update
	@$(node2nix) -- --composition node2nix.nix # -l # <- can't use -l for lockfile because lockfile version 3 not supported yet.

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
