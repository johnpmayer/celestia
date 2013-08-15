
ELM=elm --runtime elm-runtime.js

all: build/Main.html build/index.html

server: cache/Server.elmo

fresh: clean all

SOURCES=*.elm

ELM_VER=$(shell ghc-pkg latest Elm)

RUNTIME="build/elm-runtime.js"

${RUNTIME}: build
	cp ${HOME}/.cabal/share/${ELM_VER}/elm-runtime.js build/

build/Main.html: ${RUNTIME} ${SOURCES}
	${ELM} --make Main.elm 

build/index.html: build/Main.html
	(cd build; ln -s Main.html index.html)

cache/Server.elmo: ${RUNTIME} Server.elm
	${ELM} --make Server.elm

example: Example.lhs
	ghc --make Example.lhs -o example

Structure.o: Structure.hs
	ghc Structure.hs

clean:
	rm -rf cache
	rm -rf build/*

