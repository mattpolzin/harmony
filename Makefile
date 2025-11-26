
idris2 ?= idris2
IDRIS = $(idris2)
idris2-version = $(shell $(idris2) --version | sed -En 's/Idris 2, version ([^-]+).*/\1/p')
idris2-build   = $(shell $(idris2) --version | sed -En 's/Idris 2, version [^-]+(.*)/\1/p')
idris2-minor-version = $(shell echo ${idris2-version} | sed -En 's/0\.(.*)\../\1/p')
IDRIS_LIB_DIR := $(shell ${IDRIS} --libdir)

idris-adds-version = 0.5.0

kernel = $(shell uname -s)

ifeq ($(kernel),Linux)
	ised = sed -i''
else
	ised = sed -I ''
endif

.PHONY: all deps build test nix-build install package publish clean version manpage harmony

all: deps build

./depends/idris-adds:
	mkdir -p depends/idris-adds-${idris-adds-version}
	mkdir -p build/deps
	cd ./build/deps && \
  git clone https://github.com/mattpolzin/idris-adds.git && \
	cd idris-adds && \
	git checkout ${idris-adds-version} && \
	make && \
	cp -R ./build/ttc/* ../../../depends/idris-adds-${idris-adds-version}

./depends/idris2-elab-util:
	mkdir -p ./build/deps
	mkdir -p ./depends
	cd ./build/deps && \
	git clone https://github.com/stefan-hoeck/idris2-elab-util.git && \
	cd idris2-elab-util && \
	$(IDRIS) --build elab-util.ipkg && \
	IDRIS2_PACKAGE_PATH=$(IDRIS_LIB_DIR) IDRIS2_PREFIX=../../../depends $(IDRIS) --install elab-util.ipkg && \
	mv ../../../depends/idris2*/* ../../../depends/

./depends/idris2-algebra:
	mkdir -p ./build/deps
	mkdir -p ./depends
	cd ./build/deps && \
	git clone https://github.com/stefan-hoeck/idris2-algebra.git && \
	cd idris2-algebra && \
	$(IDRIS) --build algebra.ipkg && \
	IDRIS2_PACKAGE_PATH=$(IDRIS_LIB_DIR) IDRIS2_PREFIX=../../../depends $(IDRIS) --install algebra.ipkg && \
	mv ../../../depends/idris2*/* ../../../depends/

./depends/idris2-ref1:
	mkdir -p ./build/deps
	mkdir -p ./depends
	cd ./build/deps && \
	git clone https://github.com/stefan-hoeck/idris2-ref1.git && \
	cd idris2-ref1 && \
	$(IDRIS) --build ref1.ipkg && \
	IDRIS2_PACKAGE_PATH=$(IDRIS_LIB_DIR) IDRIS2_PREFIX=../../../depends $(IDRIS) --install ref1.ipkg && \
	mv ../../../depends/idris2*/* ../../../depends/

./depends/idris2-array:
	mkdir -p ./build/deps
	mkdir -p ./depends
	cd ./build/deps && \
	git clone https://github.com/stefan-hoeck/idris2-array.git && \
	cd idris2-array && \
	IDRIS2_PACKAGE_PATH="$(IDRIS_LIB_DIR):../../../depends" $(IDRIS) --build array.ipkg && \
  IDRIS2_PACKAGE_PATH="$(IDRIS_LIB_DIR):../../../depends" IDRIS2_PREFIX=../../../depends $(IDRIS) --install array.ipkg && \
	mv ../../../depends/idris2*/* ../../../depends/

./depends/idris2-bytestring:
	mkdir -p ./build/deps
	mkdir -p ./depends
	cd ./build/deps && \
	git clone https://github.com/stefan-hoeck/idris2-bytestring.git && \
	cd idris2-bytestring && \
	IDRIS2_PACKAGE_PATH="$(IDRIS_LIB_DIR):../../../depends" $(IDRIS) --build bytestring.ipkg && \
  IDRIS2_PACKAGE_PATH="$(IDRIS_LIB_DIR):../../../depends" IDRIS2_PREFIX=../../../depends $(IDRIS) --install bytestring.ipkg && \
	mv ../../../depends/idris2*/* ../../../depends/

./depends/idris2-refined:
	mkdir -p ./build/deps
	mkdir -p ./depends
	cd ./build/deps && \
	git clone https://github.com/stefan-hoeck/idris2-refined.git && \
	cd idris2-refined && \
	IDRIS2_PACKAGE_PATH="$(IDRIS_LIB_DIR):../../../depends" $(IDRIS) --build refined.ipkg && \
  IDRIS2_PACKAGE_PATH="$(IDRIS_LIB_DIR):../../../depends" IDRIS2_PREFIX=../../../depends $(IDRIS) --install refined.ipkg && \
	mv ../../../depends/idris2*/* ../../../depends/

./depends/idris2-ilex/core:
	mkdir -p ./build/deps
	mkdir -p ./depends
	cd ./build/deps && \
	git clone https://github.com/stefan-hoeck/idris2-ilex.git && \
	cd idris2-ilex/core && \
	IDRIS2_PACKAGE_PATH="$(IDRIS_LIB_DIR):../../../../depends" $(IDRIS) --build ilex-core.ipkg && \
  IDRIS2_PACKAGE_PATH="$(IDRIS_LIB_DIR):../../../../depends" IDRIS2_PREFIX=../../../../depends $(IDRIS) --install ilex-core.ipkg && \
	mv ../../../../depends/idris2*/* ../../../../depends/

./depends/idris2-ilex:
	mkdir -p ./build/deps
	mkdir -p ./depends
	cd ./build/deps && \
	cd idris2-ilex && \
	IDRIS2_PACKAGE_PATH="$(IDRIS_LIB_DIR):../../../depends" $(IDRIS) --build ilex.ipkg && \
  IDRIS2_PACKAGE_PATH="$(IDRIS_LIB_DIR):../../../depends" IDRIS2_PREFIX=../../../depends $(IDRIS) --install ilex.ipkg && \
	mv ../../../depends/idris2*/* ../../../depends/

./depends/idris2-ilex/json:
	mkdir -p ./build/deps
	mkdir -p ./depends
	cd ./build/deps && \
	cd idris2-ilex/json && \
	IDRIS2_PACKAGE_PATH="$(IDRIS_LIB_DIR):../../../../depends" $(IDRIS) --build ilex-json.ipkg && \
  IDRIS2_PACKAGE_PATH="$(IDRIS_LIB_DIR):../../../../depends" IDRIS2_PREFIX=../../../../depends $(IDRIS) --install ilex-json.ipkg && \
	mv ../../../../depends/idris2*/* ../../../../depends/

./depends/idris2-parser:
	mkdir -p ./build/deps
	mkdir -p ./depends
	cd ./build/deps && \
	git clone https://github.com/stefan-hoeck/idris2-parser.git && \
	cd idris2-parser && \
	IDRIS2_PACKAGE_PATH="$(IDRIS_LIB_DIR):../../../depends" $(IDRIS) --build parser.ipkg && \
	IDRIS2_PACKAGE_PATH="$(IDRIS_LIB_DIR):../../../depends" IDRIS2_PREFIX=../../../depends $(IDRIS) --install parser.ipkg && \
	mv ../../../depends/idris2*/* ../../../depends/

./depends/idris2-parser/json:
	mkdir -p ./build/deps
	mkdir -p ./depends
	cd ./build/deps && \
	cd idris2-parser/json && \
	IDRIS2_PACKAGE_PATH="$(IDRIS_LIB_DIR):../../../../depends" $(IDRIS) --build parser-json.ipkg && \
  IDRIS2_PACKAGE_PATH="$(IDRIS_LIB_DIR):../../../../depends" IDRIS2_PREFIX=../../../../depends $(IDRIS) --install parser-json.ipkg && \
	mv ../../../../depends/idris2*/* ../../../../depends/

./depends/idris2-json:
	mkdir -p ./build/deps
	mkdir -p ./depends
	cd ./build/deps && \
	git clone https://github.com/stefan-hoeck/idris2-json.git && \
	cd idris2-json && \
	IDRIS2_PACKAGE_PATH="$(IDRIS_LIB_DIR):../../../depends" $(IDRIS) --build json.ipkg && \
  IDRIS2_PACKAGE_PATH="$(IDRIS_LIB_DIR):../../../depends" IDRIS2_PREFIX=../../../depends $(IDRIS) --install json.ipkg && \
	mv ../../../depends/idris2*/* ../../../depends/

deps: ./depends/idris-adds ./depends/idris2-elab-util ./depends/idris2-algebra ./depends/idris2-ref1 ./depends/idris2-array ./depends/idris2-bytestring ./depends/idris2-refined ./depends/idris2-ilex/core ./depends/idris2-ilex ./depends/idris2-ilex/json ./depends/idris2-parser ./depends/idris2-parser/json ./depends/idris2-json

./node_modules/: package.json
	npm install

./build/ttc:
	make build

build: ./node_modules/
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
	$(ised) "s/Version [^ ]*/Version ${v}/" ./README.md
	@npm update
	@find . -name '*.nix' | xargs $(nix) fmt

# Remove image tags (not supported in manpages) and generate
# manpage via pandoc
manpage:
	sed 's#!\[.*\](.*)##' README.md | \
    pandoc \
    --standalone \
    --to man \
    -o man/harmony.1

package: build
	bash ./version-check.sh
	bash ./todo-check.sh
	# leave ./harmony in place
	rm -rf ./harmony-npm
	mkdir harmony-npm
	cp harmony ./harmony-npm/
	cp package.json ./harmony-npm/
	cp README.md ./harmony-npm/
	cp -R man ./harmony-npm/
	tar -czvf harmony-npm.tar.gz harmony-npm

publish : package
	npm publish ./harmony-npm

install: harmony
	npm install --global

test: ./build/ttc
	mkdir -p ./test/depends/harmony-0
	cp -R ./build/ttc/* ./test/depends/harmony-0/
	$(MAKE) -C test test

clean:
	rm -rf ./depends
	rm -rf ./build
	rm -rf ./node_modules
	rm -rf ./harmony-npm
	rm -f ./harmony
	rm -f ./harmony-npm.tar.gz
	$(MAKE) -C test clean
