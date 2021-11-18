{ config ? {}
, sourcesOverride ? {}
, pkgs
, tokenizers-haskell
}:

with pkgs;

let

  shell = pkgs.gen-tm-project.shellFor {
    name = "gen-tm-dev-shell";

    tools = {
      cabal = "latest";
      haskell-language-server = "1.3.0";
    };

    exactDeps = true;

    withHoogle = true;

    shellHook = lib.strings.concatStringsSep "\n" [
      # put tokenizers on path
      "export LD_LIBRARY_PATH=\"$LD_LIBRARY_PATH:${tokenizers-haskell}/lib\""
    ];
  };

in

  shell
