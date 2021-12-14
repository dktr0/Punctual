all: build

build:
	nix-build -o result -A ghcjs.punctual
	cp -Rf result/bin/punctual.jsexe .
	cp -f style.css punctual.jsexe/style.css

devBuild:
	cabal --ghcjs --builddir=dev-result new-build all --disable-library-profiling --disable-documentation --ghcjs-options=-DGHCJS_GC_INTERVAL=60000
	cp -Rf dev-result/build/x86_64-linux/ghcjs-8.6.0.1/punctual-0.3.5/x/punctual/build/punctual/punctual.jsexe .
	cp -f style.css punctual.jsexe/style.css

devTest:
	cabal --ghcjs new-test test:tests --disable-library-profiling --disable-documentation

ghcBuild:
	cabal --builddir=ghc-result new-build all --disable-library-profiling --disable-documentation

buildBenchmark:
	cabal --ghcjs --builddir=benchmark new-build punctual-benchmarks --disable-library-profiling --disable-documentation --ghcjs-options=-DGHCJS_GC_INTERVAL=60000

runBenchmark:
	node benchmark/build/x86_64-linux/ghcjs-8.6.0.1/punctual-0.3.5/b/punctual-benchmarks/build/punctual-benchmarks/punctual-benchmarks.jsexe/all.js

runBenchmarkInBrowser:
	open benchmark/build/x86_64-linux/ghcjs-8.6.0.1/punctual-0.3.5/b/punctual-benchmarks/build/punctual-benchmarks/punctual-benchmarks.jsexe/index.html

bundleClient:
	zip -r - ./punctual.jsexe/* > punctual-standalone.zip

serve:
	cd punctual.jsexe; python -m SimpleHTTPServer 8000

clean:
	rm -rf punctual.jsexe
	rm -rf result
	rm -rf dev-result
