
pack ?= pack
idris2 ?= $(shell $(pack) app-path idris2)
pack_package_path = $(shell pack package-path)
IDRIS2_PACKAGE_PATH := "$(pack_package_path):${IDRIS2_PACKAGE_PATH}"
IDRIS2_DATA := ./support

idris2-version = $(shell $(idris2) --version | sed -En 's/Idris 2, version ([^-]+).*/\1/p')
idris2-build   = $(shell $(idris2) --version | sed -En 's/Idris 2, version [^-]+(.*)/\1/p')
idris2-minor-version = $(shell echo ${idris2-version} | sed -En 's/0\.(.*)\../\1/p')

kernel = $(shell uname -s)

ifeq ($(kernel),Linux)
	ised = sed -i''
else
	ised = sed -I ''
endif

.PHONY: all deps build test type-test nix-build install package publish clean version readme manpage changelog harmony

all: build

deps: pack.toml
	$(pack) install-deps

./node_modules/: package.json
	npm install

./build/ttc:
	$(MAKE) build

export IDRIS2_DATA
export IDRIS2_PACKAGE_PATH
build: deps ./node_modules/
	$(idris2) --build harmony.ipkg
	@cp ./build/exec/harmony ./harmony
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
	$(ised) 's/npmDepsHash = "[^"]*"/npmDepsHash = ""/' ./default.nix
	@npm update
	@find . -name '*.nix' | xargs $(nix) fmt
	$(MAKE) readme
	$(MAKE) manpage

DOC_FILES = $(wildcard docs/*.md)

README.md: $(DOC_FILES)
	bash ./scripts/generate-readme.sh

readme: README.md

man/harmony.1: $(DOC_FILES)
	bash ./scripts/generate-manpage.sh

manpage: man/harmony.1

changelog:
	git pull --tags
	bash ./scripts/generate-changelog.sh

CHANGELOG.md: harmony.ipkg
	$(MAKE) changelog

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

FILTER ?= Test

type-test:
	find src -name 'Test.idr' | \
    grep "${FILTER}" | \
    xargs type-test --find-ipkg

clean:
	rm -rf ./build
	rm -rf ./node_modules
	rm -rf ./harmony-npm
	rm -f ./harmony
	rm -f ./harmony-npm.tar.gz
	$(MAKE) -C test clean
