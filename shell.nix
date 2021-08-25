{ config ? {}
, sourcesOverride ? {}
, pkgs
}:

with pkgs;

let

  shell = pkgs.gen-tm.shellFor {
    name = "gen-tm-dev-shell";

    tools = {
      cabal = "latest";
      haskell-language-server = "1.3.0";
    };

    exactDeps = true;

    withHoogle = true;
  };

in

  shell
