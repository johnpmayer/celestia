
ELM=elm --runtime elm-runtime.js

all: build/index.html

fresh: clean all

SOURCES=*.elm

ELM_VER=$(shell ghc-pkg latest Elm)

RUNTIME="build/elm-runtime.js"

${RUNTIME}: build
	cp ${HOME}/.cabal/share/${ELM_VER}/elm-runtime.js build/

build/index.html: build/Demo.html
	(cd build; ln -sf Demo.html index.html)

build/Demo.html: ${RUNTIME} ${SOURCES}
	${ELM} --make Demo.elm 

clean:
	rm -rf cache
	rm -rf build/*

