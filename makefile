
ELM=elm --runtime elm-runtime.js

all: build/index.html build/Demo.html

fresh: clean all

SOURCES=*.elm

ELM_VER=$(shell ghc-pkg latest Elm)

RUNTIME="build/elm-runtime.js"

${RUNTIME}: build
	cp ${HOME}/.cabal/share/${ELM_VER}/elm-runtime.js build/

build/Main.html: ${RUNTIME} ${SOURCES}
	${ELM} --make Main.elm 

build/Demo.html: ${RUNTIME} ${SOURCES}
	${ELM} --make Demo.elm 

build/index.html: build/Main.html
	(cd build; ln -sf Main.html index.html)

clean:
	rm -rf cache
	rm -rf build/*

