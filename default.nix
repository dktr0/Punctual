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

    punctual = dontCheck (dontHaddock (appendConfigureFlags super.punctual ["--ghcjs-options=-DGHCJS_BROWSER" "--ghcjs-options=-O2" "--ghcjs-options=-dedupe" "--ghcjs-options=-DGHCJS_GC_INTERVAL=60000"]));

    base-compat-batteries = dontCheck super.base-compat-batteries;

    text-show = dontCheck super.text-show;

    text-short = dontCheck super.text-short;

    criterion = dontCheck super.criterion;

    musicw =
     # dontHaddock (self.callCabal2nix "musicw" ../MusicW {});
     dontHaddock (self.callCabal2nix "musicw" (pkgs.fetchFromGitHub {
      owner = "dktr0";
      repo = "musicw";
      sha256 = "15hil1i5d089ahdp9jnwrykigcv7sdm93mlaj4laxwa49amjl4n0";
      rev = "a765aaab02541072960184151c9f35f7a8c35328";
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
      sha256 = "16fcmajzkmhz77yfyqyc08b1lz1dzy7krsl23a5v0qblgk50bsvf";
      rev = "88981a68d0486d75fcb57e97c9bb89bd5dba19f5";
      }) {});

    tempi = dontHaddock (self.callCabal2nix "tempi" (pkgs.fetchFromGitHub {
        owner = "dktr0";
        repo = "tempi";
        sha256 = "0z4fjdnl7riivw77pl8wypw1a98av3nhpmw0z5g2a1q2kjja0sfp";
        rev = "9513df2ed323ebaff9b85b72215a1e726ede1e96";
     }) {});

  };

})
