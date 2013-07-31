
ELM=elm --runtime elm-runtime.js

all: example WS.html Structure.o

SOURCES=Draw.elm  Main.elm  Physics.elm  Types.elm

ELM_VER=0.8.0.3

elm-runtime.js:
	cp ${HOME}/.cabal/share/Elm-${ELM_VER}/elm-runtime.js .

Main.html: elm-runtime.js ${SOURCES}
	${ELM} --make Main.elm 

WS.html: elm-runtime.js WS.elm
	${ELM} --make WS.elm

example: Example.lhs
	ghc --make Example.lhs -o example

Structure.o: Structure.hs
	ghc Structure.hs

clean:
	rm -f *.js *.html *.hi *.o
