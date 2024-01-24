all: build

build:
	nix-build -o result -A ghcjs.punctual
	cp -f result/bin/punctual.jsexe/index.html .
	cp -f result/bin/punctual.jsexe/rts.js .
	cp -f result/bin/punctual.jsexe/lib.js .
	cp -f result/bin/punctual.jsexe/out.js .
	cp -f result/bin/punctual.jsexe/runmain.js .
	
serve:
	python3 -m http.server 8000
	
bundleClient:
	-rm -f punctual-standalone.zip
	-rm -rf punctual-standalone
	mkdir punctual-standalone
	cp result/bin/punctual.jsexe/index.html punctual-standalone
	cp result/bin/punctual.jsexe/rts.js punctual-standalone
	cp result/bin/punctual.jsexe/lib.js punctual-standalone
	cp result/bin/punctual.jsexe/out.js punctual-standalone
	cp result/bin/punctual.jsexe/runmain.js punctual-standalone
	cp style.css punctual-standalone
	zip -r9 punctual-standalone.zip punctual-standalone/*
	rm -rf punctual-standalone
		
clean:
	rm -rf punctual.jsexe
	rm -rf result
	rm -rf dev-result
	rm -rf dist
	rm -rf dist-newstyle
	
	
devBuild:
	cabal --ghcjs --builddir=dev-result new-build all --disable-library-profiling --disable-documentation --ghcjs-options=-DGHCJS_GC_INTERVAL=60000
	cp -f dev-result/build/x86_64-linux/ghcjs-8.6.0.1/punctual-0.4.4.9/x/punctual/build/punctual/punctual.jsexe/index.html .
	cp -f dev-result/build/x86_64-linux/ghcjs-8.6.0.1/punctual-0.4.4.9/x/punctual/build/punctual/punctual.jsexe/rts.js .
	cp -f dev-result/build/x86_64-linux/ghcjs-8.6.0.1/punctual-0.4.4.9/x/punctual/build/punctual/punctual.jsexe/lib.js .
	cp -f dev-result/build/x86_64-linux/ghcjs-8.6.0.1/punctual-0.4.4.9/x/punctual/build/punctual/punctual.jsexe/out.js .
	cp -f dev-result/build/x86_64-linux/ghcjs-8.6.0.1/punctual-0.4.4.9/x/punctual/build/punctual/punctual.jsexe/runmain.js .
	
devTest:
	cabal --ghcjs new-test test:tests --disable-library-profiling --disable-documentation

ghcBuild:
	cabal --builddir=ghc-result new-build all --disable-library-profiling --disable-documentation

buildBenchmark:
	cabal --ghcjs --builddir=benchmark new-build punctual-benchmarks --disable-library-profiling --disable-documentation --ghcjs-options=-DGHCJS_GC_INTERVAL=60000

runBenchmark:
	node benchmark/build/x86_64-linux/ghcjs-8.6.0.1/punctual-0.4.4.9/b/punctual-benchmarks/build/punctual-benchmarks/punctual-benchmarks.jsexe/all.js

runBenchmarkInBrowser:
	open benchmark/build/x86_64-linux/ghcjs-8.6.0.1/punctual-0.4.4.9/b/punctual-benchmarks/build/punctual-benchmarks/punctual-benchmarks.jsexe/index.html

