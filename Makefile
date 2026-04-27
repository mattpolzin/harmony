
IDRIS ?= idris2
idris2-version = $(shell $(IDRIS) --version | sed -En 's/Idris 2, version ([^-]+).*/\1/p')
idris2-build   = $(shell $(IDRIS) --version | sed -En 's/Idris 2, version [^-]+(.*)/\1/p')
idris2-minor-version = $(shell echo ${idris2-version} | sed -En 's/0\.(.*)\../\1/p')
IDRIS_LIB_DIR := $(shell ${IDRIS} --libdir)

idris-adds-version = 0.5.0
type-test-version = 0.1.1

kernel = $(shell uname -s)

ifeq ($(kernel),Linux)
	ised = sed -i''
else
	ised = sed -I ''
endif

.PHONY: all deps build test nix-build install package publish clean version readme manpage changelog harmony

all: deps build

./build/deps/idris-adds:
	mkdir -p build/deps
	cd ./build/deps && \
  git clone https://github.com/mattpolzin/idris-adds.git

./depends/idris-adds: ./build/deps/idris-adds
	mkdir -p depends/idris-adds-${idris-adds-version}
	cd ./build/deps/idris-adds && \
	git checkout ${idris-adds-version} && \
	make && \
	cp -R ./build/ttc/* ../../../depends/idris-adds-${idris-adds-version}

./build/deps/idris2-elab-util:
	mkdir -p ./build/deps
	cd ./build/deps && \
	git clone https://github.com/stefan-hoeck/idris2-elab-util.git

./depends/idris2-elab-util: ./build/deps/idris2-elab-util
	mkdir -p ./depends
	cd ./build/deps/idris2-elab-util && \
	$(IDRIS) --build elab-util.ipkg && \
	IDRIS2_PACKAGE_PATH=$(IDRIS_LIB_DIR) IDRIS2_PREFIX=../../../depends $(IDRIS) --install elab-util.ipkg && \
	rm -rf ../../../depends/elab-util-* && \
	mv ../../../depends/idris2*/* ../../../depends/

./depends/idris2-elab-pretty: ./build/deps/idris2-elab-util
	mkdir -p ./depends
	cd ./build/deps/idris2-elab-util && \
	IDRIS2_PACKAGE_PATH="$(IDRIS_LIB_DIR):../../../depends" $(IDRIS) --build elab-pretty.ipkg && \
  IDRIS2_PACKAGE_PATH="$(IDRIS_LIB_DIR):../../../depends" IDRIS2_PREFIX=../../../depends $(IDRIS) --install elab-pretty.ipkg && \
	rm -rf ../../../depends/elab-pretty-* && \
	mv ../../../depends/idris2*/* ../../../depends/

./build/deps/idris2-algebra:
	mkdir -p ./build/deps
	cd ./build/deps && \
	git clone https://github.com/stefan-hoeck/idris2-algebra.git

./depends/idris2-algebra: ./build/deps/idris2-algebra
	mkdir -p ./depends
	cd ./build/deps/idris2-algebra && \
	$(IDRIS) --build algebra.ipkg && \
	IDRIS2_PACKAGE_PATH=$(IDRIS_LIB_DIR) IDRIS2_PREFIX=../../../depends $(IDRIS) --install algebra.ipkg && \
	rm -rf ../../../depends/algebra-* && \
	mv ../../../depends/idris2*/* ../../../depends/

./build/deps/idris2-ref1:
	mkdir -p ./build/deps
	cd ./build/deps && \
	git clone https://github.com/stefan-hoeck/idris2-ref1.git

./depends/idris2-ref1: ./build/deps/idris2-ref1
	mkdir -p ./depends
	cd ./build/deps/idris2-ref1 && \
	$(IDRIS) --build ref1.ipkg && \
	IDRIS2_PACKAGE_PATH=$(IDRIS_LIB_DIR) IDRIS2_PREFIX=../../../depends $(IDRIS) --install ref1.ipkg && \
	rm -rf ../../../depends/ref1-* && \
	mv ../../../depends/idris2*/* ../../../depends/

./build/deps/idris2-array:
	mkdir -p ./build/deps
	cd ./build/deps && \
	git clone https://github.com/stefan-hoeck/idris2-array.git

./depends/idris2-array: ./build/deps/idris2-array
	mkdir -p ./depends
	cd ./build/deps/idris2-array && \
	IDRIS2_PACKAGE_PATH="$(IDRIS_LIB_DIR):../../../depends" $(IDRIS) --build array.ipkg && \
  IDRIS2_PACKAGE_PATH="$(IDRIS_LIB_DIR):../../../depends" IDRIS2_PREFIX=../../../depends $(IDRIS) --install array.ipkg && \
	rm -rf ../../../depends/array-* && \
	mv ../../../depends/idris2*/* ../../../depends/

./build/deps/idris2-bytestring:
	mkdir -p ./build/deps
	cd ./build/deps && \
	git clone https://github.com/stefan-hoeck/idris2-bytestring.git

./depends/idris2-bytestring: ./build/deps/idris2-bytestring
	mkdir -p ./build/deps
	mkdir -p ./depends
	cd ./build/deps/idris2-bytestring && \
	IDRIS2_PACKAGE_PATH="$(IDRIS_LIB_DIR):../../../depends" $(IDRIS) --build bytestring.ipkg && \
  IDRIS2_PACKAGE_PATH="$(IDRIS_LIB_DIR):../../../depends" IDRIS2_PREFIX=../../../depends $(IDRIS) --install bytestring.ipkg && \
	rm -rf ../../../depends/bytestring-* && \
	mv ../../../depends/idris2*/* ../../../depends/

./build/deps/idris2-refined:
	mkdir -p ./build/deps
	cd ./build/deps && \
	git clone https://github.com/stefan-hoeck/idris2-refined.git

./depends/idris2-refined: ./build/deps/idris2-refined
	mkdir -p ./depends
	cd ./build/deps/idris2-refined && \
	IDRIS2_PACKAGE_PATH="$(IDRIS_LIB_DIR):../../../depends" $(IDRIS) --build refined.ipkg && \
  IDRIS2_PACKAGE_PATH="$(IDRIS_LIB_DIR):../../../depends" IDRIS2_PREFIX=../../../depends $(IDRIS) --install refined.ipkg && \
	rm -rf ../../../depends/refined-* && \
	mv ../../../depends/idris2*/* ../../../depends/

./build/deps/idris2-ilex:
	mkdir -p ./build/deps
	cd ./build/deps && \
	git clone https://github.com/stefan-hoeck/idris2-ilex.git

./depends/idris2-ilex/core: ./build/deps/idris2-ilex
	mkdir -p ./depends
	cd ./build/deps/idris2-ilex/core && \
	IDRIS2_PACKAGE_PATH="$(IDRIS_LIB_DIR):../../../../depends" $(IDRIS) --build ilex-core.ipkg && \
  IDRIS2_PACKAGE_PATH="$(IDRIS_LIB_DIR):../../../../depends" IDRIS2_PREFIX=../../../../depends $(IDRIS) --install ilex-core.ipkg && \
	rm -rf ../../../../depends/ilex-core-* && \
	mv ../../../../depends/idris2*/* ../../../../depends/

./depends/idris2-ilex: ./build/deps/idris2-ilex
	mkdir -p ./depends
	cd ./build/deps/idris2-ilex && \
	IDRIS2_PACKAGE_PATH="$(IDRIS_LIB_DIR):../../../depends" $(IDRIS) --build ilex.ipkg && \
  IDRIS2_PACKAGE_PATH="$(IDRIS_LIB_DIR):../../../depends" IDRIS2_PREFIX=../../../depends $(IDRIS) --install ilex.ipkg && \
	rm -rf ../../../depends/ilex-0* && \
	mv ../../../depends/idris2*/* ../../../depends/

./depends/idris2-ilex/json: ./build/deps/idris2-ilex
	mkdir -p ./depends
	cd ./build/deps/idris2-ilex/json && \
	IDRIS2_PACKAGE_PATH="$(IDRIS_LIB_DIR):../../../../depends" $(IDRIS) --build ilex-json.ipkg && \
  IDRIS2_PACKAGE_PATH="$(IDRIS_LIB_DIR):../../../../depends" IDRIS2_PREFIX=../../../../depends $(IDRIS) --install ilex-json.ipkg && \
	rm -rf ../../../../depends/ilex-json-* && \
	mv ../../../../depends/idris2*/* ../../../../depends/

./build/deps/idris2-parser:
	mkdir -p ./build/deps
	cd ./build/deps && \
	git clone https://github.com/stefan-hoeck/idris2-parser.git

./depends/idris2-parser: ./build/deps/idris2-parser
	mkdir -p ./depends
	cd ./build/deps/idris2-parser && \
	IDRIS2_PACKAGE_PATH="$(IDRIS_LIB_DIR):../../../depends" $(IDRIS) --build parser.ipkg && \
	IDRIS2_PACKAGE_PATH="$(IDRIS_LIB_DIR):../../../depends" IDRIS2_PREFIX=../../../depends $(IDRIS) --install parser.ipkg && \
	rm -rf ../../../depends/parser-* && \
	mv ../../../depends/idris2*/* ../../../depends/

./depends/idris2-parser/json: ./build/deps/idris2-parser
	mkdir -p ./depends
	cd ./build/deps/idris2-parser/json && \
	IDRIS2_PACKAGE_PATH="$(IDRIS_LIB_DIR):../../../../depends" $(IDRIS) --build parser-json.ipkg && \
  IDRIS2_PACKAGE_PATH="$(IDRIS_LIB_DIR):../../../../depends" IDRIS2_PREFIX=../../../../depends $(IDRIS) --install parser-json.ipkg && \
	rm -rf ../../../../depends/parser-json-* && \
	mv ../../../../depends/idris2*/* ../../../../depends/

./depends/idris2-parser/show: ./build/deps/idris2-parser
	mkdir -p ./depends
	cd ./build/deps/idris2-parser/show && \
	IDRIS2_PACKAGE_PATH="$(IDRIS_LIB_DIR):../../../../depends" $(IDRIS) --build parser-show.ipkg && \
  IDRIS2_PACKAGE_PATH="$(IDRIS_LIB_DIR):../../../../depends" IDRIS2_PREFIX=../../../../depends $(IDRIS) --install parser-show.ipkg && \
	rm -rf ../../../../depends/parser-show-* && \
	mv ../../../../depends/idris2*/* ../../../../depends/

./build/deps/idris2-json:
	mkdir -p ./build/deps
	cd ./build/deps && \
	git clone https://github.com/stefan-hoeck/idris2-json.git

./depends/idris2-json: ./build/deps/idris2-json
	mkdir -p ./depends
	cd ./build/deps/idris2-json && \
	IDRIS2_PACKAGE_PATH="$(IDRIS_LIB_DIR):../../../depends" $(IDRIS) --build json.ipkg && \
  IDRIS2_PACKAGE_PATH="$(IDRIS_LIB_DIR):../../../depends" IDRIS2_PREFIX=../../../depends $(IDRIS) --install json.ipkg && \
	rm -rf ../../../depends/json-* && \
	mv ../../../depends/idris2*/* ../../../depends/

./build/deps/idris2-random-pure:
	mkdir -p ./build/deps
	cd ./build/deps && \
	git clone https://github.com/buzden/idris2-random-pure.git

./depends/idris2-random-pure: ./build/deps/idris2-random-pure
	mkdir -p ./depends
	cd ./build/deps/idris2-random-pure && \
	IDRIS2_PACKAGE_PATH="$(IDRIS_LIB_DIR):../../../depends" $(IDRIS) --build random-pure.ipkg && \
  IDRIS2_PACKAGE_PATH="$(IDRIS_LIB_DIR):../../../depends" IDRIS2_PREFIX=../../../depends $(IDRIS) --install random-pure.ipkg && \
	rm -rf ../../../depends/random-pure-* && \
	mv ../../../depends/idris2*/* ../../../depends/

./build/deps/idris2-weaker-traversals:
	mkdir -p ./build/deps
	cd ./build/deps && \
	git clone https://github.com/buzden/idris2-weaker-traversals.git

./depends/idris2-weaker-traversals: ./build/deps/idris2-weaker-traversals
	mkdir -p ./depends
	cd ./build/deps/idris2-weaker-traversals && \
	IDRIS2_PACKAGE_PATH="$(IDRIS_LIB_DIR):../../../depends" $(IDRIS) --build weaker-traversals.ipkg && \
  IDRIS2_PACKAGE_PATH="$(IDRIS_LIB_DIR):../../../depends" IDRIS2_PREFIX=../../../depends $(IDRIS) --install weaker-traversals.ipkg && \
	rm -rf ../../../depends/weaker-traversals-* && \
	mv ../../../depends/idris2*/* ../../../depends/

./build/deps/idris2-bounded-doubles:
	mkdir -p ./build/deps
	cd ./build/deps && \
	git clone https://github.com/buzden/idris2-bounded-doubles.git

./depends/idris2-bounded-doubles: ./build/deps/idris2-bounded-doubles
	mkdir -p ./depends
	cd ./build/deps/idris2-bounded-doubles && \
	IDRIS2_PACKAGE_PATH="$(IDRIS_LIB_DIR):../../../depends" $(IDRIS) --build bounded-doubles.ipkg && \
  IDRIS2_PACKAGE_PATH="$(IDRIS_LIB_DIR):../../../depends" IDRIS2_PREFIX=../../../depends $(IDRIS) --install bounded-doubles.ipkg && \
	rm -rf ../../../depends/bounded-doubles-* && \
	mv ../../../depends/idris2*/* ../../../depends/

./build/deps/idris2-summary-stat:
	mkdir -p ./build/deps
	cd ./build/deps && \
	git clone https://github.com/buzden/idris2-summary-stat.git

./depends/idris2-summary-stat: ./build/deps/idris2-summary-stat
	mkdir -p ./depends
	cd ./build/deps/idris2-summary-stat && \
	IDRIS2_PACKAGE_PATH="$(IDRIS_LIB_DIR):../../../depends" $(IDRIS) --build summary-stat.ipkg && \
  IDRIS2_PACKAGE_PATH="$(IDRIS_LIB_DIR):../../../depends" IDRIS2_PREFIX=../../../depends $(IDRIS) --install summary-stat.ipkg && \
	rm -rf ../../../depends/summary-stat-* && \
	mv ../../../depends/idris2*/* ../../../depends/

./build/deps/idris2-getopts:
	mkdir -p ./build/deps
	cd ./build/deps && \
	git clone https://github.com/idris-community/idris2-getopts.git

./depends/idris2-getopts: ./build/deps/idris2-getopts
	mkdir -p ./depends
	cd ./build/deps/idris2-getopts && \
	IDRIS2_PACKAGE_PATH="$(IDRIS_LIB_DIR):../../../depends" $(IDRIS) --build getopts.ipkg && \
  IDRIS2_PACKAGE_PATH="$(IDRIS_LIB_DIR):../../../depends" IDRIS2_PREFIX=../../../depends $(IDRIS) --install getopts.ipkg && \
	rm -rf ../../../depends/getopts-* && \
	mv ../../../depends/idris2*/* ../../../depends/

./build/deps/prettier:
	mkdir -p ./build/deps
	cd ./build/deps && \
	git clone https://github.com/z-snails/prettier.git

./depends/prettier: ./build/deps/prettier
	mkdir -p ./depends
	cd ./build/deps/prettier && \
	IDRIS2_PACKAGE_PATH="$(IDRIS_LIB_DIR):../../../depends" $(IDRIS) --build prettier.ipkg && \
  IDRIS2_PACKAGE_PATH="$(IDRIS_LIB_DIR):../../../depends" IDRIS2_PREFIX=../../../depends $(IDRIS) --install prettier.ipkg && \
	rm -rf ../../../depends/prettier-* && \
	mv ../../../depends/idris2*/* ../../../depends/

./build/deps/idris2-ansi:
	mkdir -p ./build/deps
	cd ./build/deps && \
	git clone https://github.com/idris-community/idris2-ansi.git

./depends/idris2-ansi: ./build/deps/idris2-ansi
	mkdir -p ./depends
	cd ./build/deps/idris2-ansi && \
	IDRIS2_PACKAGE_PATH="$(IDRIS_LIB_DIR):../../../depends" $(IDRIS) --build ansi.ipkg && \
  IDRIS2_PACKAGE_PATH="$(IDRIS_LIB_DIR):../../../depends" IDRIS2_PREFIX=../../../depends $(IDRIS) --install ansi.ipkg && \
	rm -rf ../../../depends/ansi-* && \
	mv ../../../depends/idris2*/* ../../../depends/

./depends/idris2-prettier-ansi: ./build/deps/idris2-ansi
	mkdir -p ./depends
	cd ./build/deps/idris2-ansi/prettier && \
	IDRIS2_PACKAGE_PATH="$(IDRIS_LIB_DIR):../../../../depends" $(IDRIS) --build prettier-ansi.ipkg && \
  IDRIS2_PACKAGE_PATH="$(IDRIS_LIB_DIR):../../../../depends" IDRIS2_PREFIX=../../../../depends $(IDRIS) --install prettier-ansi.ipkg && \
	rm -rf ../../../../depends/prettier-ansi-* && \
	mv ../../../../depends/idris2*/* ../../../../depends/

./build/deps/idris2-sop:
	mkdir -p ./build/deps
	cd ./build/deps && \
	git clone https://github.com/stefan-hoeck/idris2-sop.git

./depends/idris2-sop: ./build/deps/idris2-sop
	mkdir -p ./depends
	cd ./build/deps/idris2-sop && \
	IDRIS2_PACKAGE_PATH="$(IDRIS_LIB_DIR):../../../depends" $(IDRIS) --build sop.ipkg && \
  IDRIS2_PACKAGE_PATH="$(IDRIS_LIB_DIR):../../../depends" IDRIS2_PREFIX=../../../depends $(IDRIS) --install sop.ipkg && \
	rm -rf ../../../depends/sop-* && \
	mv ../../../depends/idris2*/* ../../../depends/

./build/deps/idris2-pretty-show:
	mkdir -p ./build/deps
	cd ./build/deps && \
	git clone https://github.com/stefan-hoeck/idris2-pretty-show.git

./depends/idris2-pretty-show: ./build/deps/idris2-pretty-show
	mkdir -p ./depends
	cd ./build/deps/idris2-pretty-show && \
	IDRIS2_PACKAGE_PATH="$(IDRIS_LIB_DIR):../../../depends" $(IDRIS) --build pretty-show.ipkg && \
  IDRIS2_PACKAGE_PATH="$(IDRIS_LIB_DIR):../../../depends" IDRIS2_PREFIX=../../../depends $(IDRIS) --install pretty-show.ipkg && \
	rm -rf ../../../depends/pretty-show-* && \
	mv ../../../depends/idris2*/* ../../../depends/

./build/deps/idris2-hedgehog:
	mkdir -p ./build/deps
	cd ./build/deps && \
	git clone https://github.com/stefan-hoeck/idris2-hedgehog.git

./depends/idris2-hedgehog: ./build/deps/idris2-hedgehog
	mkdir -p ./depends
	cd ./build/deps/idris2-hedgehog && \
	IDRIS2_PACKAGE_PATH="$(IDRIS_LIB_DIR):../../../depends" $(IDRIS) --build hedgehog.ipkg && \
  IDRIS2_PACKAGE_PATH="$(IDRIS_LIB_DIR):../../../depends" IDRIS2_PREFIX=../../../depends $(IDRIS) --install hedgehog.ipkg && \
	rm -rf ../../../depends/hedgehog-* && \
	mv ../../../depends/idris2*/* ../../../depends/

./build/deps/idris2-type-test:
	mkdir -p build/deps
	cd ./build/deps && \
  git clone https://github.com/mattpolzin/idris2-type-test.git

./depends/idris2-type-test-api: ./build/deps/idris2-type-test
	mkdir -p depends/idris2-type-test-api-${type-test-version}
	cd ./build/deps/idris2-type-test/type-test-api && \
	git checkout ${type-test-version} && \
	IDRIS2_PACKAGE_PATH="$(IDRIS_LIB_DIR):../../../../depends" $(IDRIS) --build type-test-api.ipkg && \
	IDRIS2_PACKAGE_PATH="$(IDRIS_LIB_DIR):../../../../depends" IDRIS2_PREFIX=../../../../depends $(IDRIS) --install type-test-api.ipkg && \
	rm -rf ../../../../depends/type-test-api-* && \
	mv ../../../../depends/idris2*/* ../../../../depends/

deps: ./depends/idris-adds ./depends/idris2-elab-util ./depends/idris2-algebra ./depends/idris2-ref1 ./depends/idris2-array ./depends/idris2-bytestring ./depends/idris2-refined ./depends/idris2-ilex/core ./depends/idris2-ilex ./depends/idris2-ilex/json ./depends/idris2-parser ./depends/idris2-parser/json ./depends/idris2-json ./depends/idris2-getopts ./depends/idris2-ansi ./depends/prettier ./depends/idris2-elab-pretty ./depends/idris2-prettier-ansi ./depends/idris2-parser/show ./depends/idris2-pretty-show ./depends/idris2-random-pure ./depends/idris2-sop ./depends/idris2-bounded-doubles ./depends/idris2-weaker-traversals ./depends/idris2-summary-stat ./depends/idris2-hedgehog ./depends/idris2-type-test-api

./node_modules/: package.json
	npm install

./build/ttc:
	make build

build: ./node_modules/
	IDRIS2_DATA=./support $(IDRIS) --build harmony.ipkg
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
	$(MAKE) clean
	$(nix) build .

version:
	@(if [[ "${v}" == '' ]]; then echo "please set the 'v' variable."; exit 1; fi)
	$(ised) "s/version = .*/version = ${v}/" ./harmony.ipkg
	$(ised) "s/appVersion = \".*\"/appVersion = \"${v}\"/" ./src/AppVersion.idr
	$(ised) "s/\"version\": \".*\"/\"version\": \"${v}\"/" ./package.json
	$(ised) "s/Version [^ ]*/Version ${v}/" ./docs/_0_0_MANPAGE_HEADER.md
	@npm update
	@find . -name '*.nix' | xargs $(nix) fmt
	$(MAKE) manpage

README_FILES = $(wildcard docs/*.md)

README.md: $(README_FILES)
	bash ./scripts/generate-readme.sh

readme: README.md

man/harmony.1: $(README_FILES)
	bash ./scripts/generate-manpage.sh

manpage: man/harmony.1

changelog:
	bash ./scripts/generate-changelog.sh

package: build README.md man/harmony.1
	bash ./scripts/version-check.sh
	bash ./scripts/todo-check.sh
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
