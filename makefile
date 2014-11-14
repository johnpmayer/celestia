
all: build/index.html

fresh: clean all

SOURCES=*.elm

ELM_VER=$(shell ghc-pkg latest Elm)

ELM=elm --bundle-runtime ${CABINFLAGS}

build/index.html: build/Demo.html
	cp build/Demo.html build/index.html

build/Demo.html: ${SOURCES}
	${ELM} --make Demo.elm 

build/Cat.js: ${RUNTIME} ${SOURCES}
	${ELM} --only-js --node Cat.elm

clean:
	rm -rf cache
	rm -rf build/*

