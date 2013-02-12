
all: site

SOURCES=*.elm

build:
	mkdir -p build

site: build build/elm-runtime.js ${SOURCES}
	elm --output-directory=build --runtime=elm-runtime.js --make Main.elm

build/elm-runtime.js: build
	cp ~/workspace/Elm/elm/elm-runtime-0.7.2.js build/elm-runtime.js
