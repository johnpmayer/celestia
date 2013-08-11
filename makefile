
ELM=elm --runtime elm-runtime.js

all: build/WS.html build/Main.html cache/Server.elmo

fresh: clean all

SOURCES=*.elm

ELM_VER=0.9

RUNTIME="build/elm-runtime.js"

build:
	mkdir -p build

${RUNTIME}: build
	cp ${HOME}/.cabal/share/Elm-${ELM_VER}/elm-runtime.js build/

build/Main.html: ${RUNTIME} ${SOURCES}
	${ELM} --make Main.elm 

build/WS.html: ${RUNTIME} WS.elm
	${ELM} --make WS.elm

cache/Server.elmo: ${RUNTIME} Server.elm
	${ELM} --make Server.elm

example: Example.lhs
	ghc --make Example.lhs -o example

Structure.o: Structure.hs
	ghc Structure.hs

clean:
	rm -rf build cache

