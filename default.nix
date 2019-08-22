{ reflex-commit ? "716879f16d53c93766e7ed9af17416fccb2edfe1" }:

let reflex-platform = builtins.fetchTarball "https://github.com/reflex-frp/reflex-platform/archive/${reflex-commit}.tar.gz"; in

(import reflex-platform {}).project ({ pkgs, ghc8_4, ... }:

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

    base-compat-batteries = dontCheck super.base-compat-batteries;
    text-show = dontCheck super.text-show;

    musicw = dontHaddock (self.callCabal2nix "musicw" (pkgs.fetchFromGitHub {
      owner = "dktr0";
      repo = "musicw";
      rev = "0081f8abc70dc284f21b64c29d29e48156f3c21f";
      sha256 = "08kv9kgnfk1kw03z6rv48vjmz10pll7p636pk5prjpb207pjbqm8";
      }) {});

    reflex-dom-contrib = dontHaddock (self.callCabal2nix "reflex-dom-contrib" (pkgs.fetchFromGitHub {
      owner = "reflex-frp";
      repo = "reflex-dom-contrib";
      rev = "796a3f0fa1ff59cbad97c918983355b46c3b6aa0";
      sha256 = "0aqj7xm97mwxhhpcrx58bbg3hhn12jrzk13lf4zhpk2rrjw6yvmc";
      }) {});

  };

})
