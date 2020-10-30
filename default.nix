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
      sha256 = "1x0ca7ad2gs6nqbnmlpfaj049pzjn8rd2ijmlywfaihw16y4fk82";
      rev = "7ba08641af537ef0521a95a69b373d47f6263366";
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
      sha256 = "1lrw14v4n5cdk7b8la9z4bc9sh8n0496hb4s7fcbm6g7p5m8qc0j";
      rev = "bd5daf365086a4b3a75af9ad9c0b6dedf687f48a";
     }) {});

    tempi = dontHaddock (self.callCabal2nix "tempi" (pkgs.fetchFromGitHub {
        owner = "dktr0";
        repo = "tempi";
        sha256 = "0z4fjdnl7riivw77pl8wypw1a98av3nhpmw0z5g2a1q2kjja0sfp";
        rev = "9513df2ed323ebaff9b85b72215a1e726ede1e96";
     }) {});

  };

})
