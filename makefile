
all: Main.html

SOURCES=*.elm

elm-runtime.js:
	cp /home/mayerjoh/.cabal/share/Elm-0.8/elm-runtime.js .

Main.html: elm-runtime.js ${SOURCES}
	elm --runtime elm-runtime.js Main.elm 

clean:
	rm *js *html
