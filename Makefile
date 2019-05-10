all: build

build:
	stack build
	cp -Rf $$(stack path --local-install-root)/bin/punctual.jsexe .
	cp -f MusicW-audioWorklets.js punctual.jsexe
	cp -f style.css punctual.jsexe/style.css

clean:
	stack clean
	rm -rf punctual.jsexe
