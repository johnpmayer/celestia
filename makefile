
all: build/index.html

fresh: clean all

SOURCES=*.elm

RUNTIME="build/elm-runtime.js"
ELM_VER=$(shell ghc-pkg latest Elm)

${RUNTIME}: build
	cp ${HOME}/.cabal/share/${ELM_VER}/elm-runtime.js build/

CABINFLAGS=$(shell cabin flags)
ELM=elm --runtime elm-runtime.js ${CABINFLAGS}

build/index.html: build/Demo.html
	cp build/Demo.html build/index.html

build/Demo.html: ${RUNTIME} ${SOURCES}
	${ELM} --make Demo.elm 

build/Cat.js: ${RUNTIME} ${SOURCES}
	${ELM} --only-js --node Cat.elm

clean:
	rm -rf cache
	rm -rf build/*

