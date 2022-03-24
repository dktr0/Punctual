{ reflex-commit ? "123a6f487ca954fd983f6d4cd6b2a69d4c463d10" }:

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

    ghcjs-dom-jsffi = if !(self.ghc.isGhcjs or false) then null else super.ghcjs-dom-jsffi;

    punctual = dontCheck (dontHaddock (appendConfigureFlags super.punctual ["--ghcjs-options=-DGHCJS_BROWSER" "--ghcjs-options=-O2" "--ghcjs-options=-dedupe" "--ghcjs-options=-DGHCJS_GC_INTERVAL=60000"]));

    # musicw = self.callHackageDirect {
    # musicw = self.callHackage "musicw" "0.3.10" {};
    #  pkg = "musicw";
    #    ver = "0.3.10";
    #  sha256 = "05xc7jd3k45ymq3pd4q37rnxcj7z0jxmpxj6gmbp7p0n4p49cagh";
    # } { };
    # temporary hash for when you don't know the hash in the above: pkgs.lib.fakeSha256
    musicw = self.callCabal2nix "musicw" (pkgs.fetchFromGitHub {
      owner = "dktr0";
      repo = "musicw";
      sha256 = "19c31jnsdhw85qczgwcmfch217nfc631qv9jf4wzbyg0xz50s64c";
      rev = "b42ff7b1ea3ea322fb830a4e4757626761aebcd5";
    }) {};
    # musicw = if (!self.ghc.isGhcjs or false) then null else self.callCabal2nix "musicw" ../MusicW {};

    haskellish = self.callCabal2nix "haskellish" (pkgs.fetchFromGitHub {
      owner = "dktr0";
      repo = "Haskellish";
      sha256 = "0w04qg2jmjgrkxrjc9ddads606fvsbain9bf2k1d0spk41ckayyb";
      rev = "3bc2dd4133843751a3ae4a3e88063d7d99f18f81";
    }) {};

    tempi = self.callCabal2nix "tempi" (pkgs.fetchFromGitHub {
      owner = "dktr0";
      repo = "tempi";
      sha256 = "0z4fjdnl7riivw77pl8wypw1a98av3nhpmw0z5g2a1q2kjja0sfp";
      rev = "9513df2ed323ebaff9b85b72215a1e726ede1e96";
    }) {};

  };

})
