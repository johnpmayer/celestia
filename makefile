
ELM=elm --runtime elm-runtime.js

all: example WS.html Ship.o

SOURCES=Draw.elm  Main.elm  Physics.elm  Types.elm

elm-runtime.js:
	cp ${HOME}/.cabal/share/Elm-0.8/elm-runtime.js .

Main.html: elm-runtime.js ${SOURCES}
	${ELM} --make Main.elm 

WS.html: elm-runtime.js WS.elm
	${ELM} --make WS.elm

example: Example.lhs
	ghc --make Example.lhs -o example

Ship.o: Ship.hs
	ghc Ship.hs

clean:
	rm -f *.js *.html *.hi *.o
