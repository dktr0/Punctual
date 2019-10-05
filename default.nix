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

    base-compat-batteries = dontCheck super.base-compat-batteries;
    text-show = dontCheck super.text-show;

    musicw = dontHaddock (self.callCabal2nix "musicw" (pkgs.fetchFromGitHub {
      owner = "dktr0";
      repo = "musicw";
      rev = "f8915f856d75bafc3e8853e5c95b9145c7742598";
      sha256 = "0fmqi4bblik0y2j5pazh3k6qa40vanwrbr7zahi1hcxc0vcfxz33";
      }) {});

    reflex-dom-contrib = dontHaddock (self.callCabal2nix "reflex-dom-contrib" (pkgs.fetchFromGitHub {
      owner = "reflex-frp";
      repo = "reflex-dom-contrib";
      rev = "b9e2965dff062a4e13140f66d487362a34fe58b3";
      sha256 = "1aa045mr82hdzzd8qlqhfrycgyhd29lad8rf7vsqykly9axpl52a";
      }) {});

  };

})
