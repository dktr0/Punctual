all: build

build:
	stack build
	cp -Rf $$(stack path --local-install-root)/bin/punctual.jsexe .
	cp -f style.css punctual.jsexe/style.css

serve:
	cd punctual.jsexe; python -m SimpleHTTPServer 8000

clean:
	stack clean
	rm -rf punctual.jsexe
