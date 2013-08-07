
ELM=elm --runtime elm-runtime.js

all: clean WS.html Main.html

SOURCES=Draw.elm Main.elm Physics.elm Types.elm

ELM_VER=0.8.0.3

RUNTIME="ElmFiles/elm-runtime.js"

ElmFiles:
	mkdir -p ElmFiles

${RUNTIME}: ElmFiles
	cp ${HOME}/.cabal/share/Elm-${ELM_VER}/elm-runtime.js ElmFiles/

Main.html: ${RUNTIME} ${SOURCES}
	${ELM} --make Main.elm 

WS.html: ${RUNTIME} WS.elm
	${ELM} --make WS.elm

example: Example.lhs
	ghc --make Example.lhs -o example

Structure.o: Structure.hs
	ghc Structure.hs

clean:
	rm -rf ElmFiles

