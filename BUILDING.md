To build the Punctual standalone, you must have a working Nix installation, as well as the standard 'make' build tool.

First, enter Punctual's Nix/GHCJS shell:

```
cd ~/Punctual
nix-shell -A shells.ghcjs
```

Now you can do the development build of Punctual as follows (this uses cabal inside the Nix shell to build the Punctual executable):

```
make devBuild
```

Alternately (for example, when everything is working), you can do an optimized release build instead. (This is not recommended during development as it will always recompile everything if anything anywhere has changed. It uses nix-build to build the Punctual executable.)

```
make build
```

The results of a successful build (release or development) are placed in the punctual.jsexe folder under the main Punctual folder. However, because of the security policies around audio worklets (which Punctual uses heavily), the files in that folder must be served to the browser by a web server (ie. by http:) rather than loaded locally (ie. by file:). Assuming you have python installed, you can serve the punctual.jsexe folder locally as follows:

```
make serve
```

You can load the successful dev build in your Chrome browser now by pointing it to http://127.0.0.1:8000

Note that there are often problems with Chrome holding on to previous versions, so it is usually best to right click on Chrome's reload button and force a "hard reload" to make sure it is loading the most recent build product and not something left over from previous development/testing.

There are other recipes in the Makefile - look at the Makefile directly for other possible usages.
