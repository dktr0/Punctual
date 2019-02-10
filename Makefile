all: build install

build:
	stack build

install:
	cp -Rf $$(stack path --local-install-root)/bin/punctual.jsexe .
	cp -f style.css punctual.jsexe/style.css
