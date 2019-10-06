all: build

build:
	nix-build -o result -A ghcjs.punctual
	cp -Rf result/bin/punctual.jsexe .
	cp -f style.css punctual.jsexe/style.css

devBuild:
	cabal --ghcjs --builddir=dev-result new-build all --disable-library-profiling --disable-documentation
	cp -Rf dev-result/build/x86_64-linux/ghcjs-8.6.0.1/punctual-0.0.0.1/x/punctual/build/punctual/punctual.jsexe .
	cp -f style.css punctual.jsexe/style.css

serve:
	cd punctual.jsexe; python -m SimpleHTTPServer 8000

clean:
	rm -rf punctual.jsexe
	rm -rf result
	rm -rf dev-result
