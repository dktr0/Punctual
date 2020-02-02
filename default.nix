{ reflex-commit ? "9e306f72ed0dbcdccce30a4ba0eb37aa03cf91e3" }:

let reflex-platform = builtins.fetchTarball "https://github.com/reflex-frp/reflex-platform/archive/${reflex-commit}.tar.gz"; in

(import reflex-platform {}).project ({ pkgs, ... }:

with pkgs.haskell.lib;

{

  name = "Punctual";

  packages = {
    punctual = ./.;
  };

  shells = {
    ghc = ["punctual"];
    ghcjs = ["punctual"];
  };

  android = {};

  overrides = self: super: {
    #       lens = self.callHackage "lens" "4.15.4" {}; # saving this example in case we need it later

    Glob = dontCheck super.Glob;

    punctual = appendConfigureFlags super.punctual ["--ghcjs-options=-DGHCJS_BROWSER" "--ghcjs-options=-O2" "--ghcjs-options=-dedupe" "--ghcjs-options=-DGHCJS_GC_INTERVAL=60000"];

    base-compat-batteries = dontCheck super.base-compat-batteries;

    text-show = dontCheck super.text-show;

    text-short = dontCheck super.text-short;

    criterion = dontCheck super.criterion;

    musicw =
    #dontHaddock (self.callCabal2nix "musicw" ../MusicW {});
    dontHaddock (self.callCabal2nix "musicw" (pkgs.fetchFromGitHub {
      owner = "dktr0";
      repo = "musicw";
      sha256 = "1j4bjb7jv9qgaw5cks6aaz9ji9y1nk4wpca3kmrz65wlx81cpfkk";
      rev = "347ad516136b4dc61cea7dddfbffb014fb6aa9c1";
      }) {});

    reflex-dom-contrib = dontHaddock (self.callCabal2nix "reflex-dom-contrib" (pkgs.fetchFromGitHub {
      owner = "reflex-frp";
      repo = "reflex-dom-contrib";
      rev = "b9e2965dff062a4e13140f66d487362a34fe58b3";
      sha256 = "1aa045mr82hdzzd8qlqhfrycgyhd29lad8rf7vsqykly9axpl52a";
      }) {});

    haskellish = dontHaddock # (self.callCabal2nix "haskellish" ../haskellish {});
     (self.callCabal2nix "haskellish" (pkgs.fetchFromGitHub {
      owner = "dktr0";
      repo = "Haskellish";
      sha256 = "05dn0lyyaagljibxf0pnlin9ni1c5v7kh9c4bs76ch9z5wsk9nf1";
      rev = "a1d78aedde218f8429517d8cfc599f9d7bbbc2d3";
     }) {});

  };

})
