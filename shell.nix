let

  project = import ./nix/default.nix {};

in

  project.shellFor {
    withHoogle = true;

    tools = {
      cabal = "latest";
      hlint = "latest";
      haskell-language-server = "latest";
    };

    buildInputs = [ (import <nixpkgs> {}).git ];

    exactDeps = true;
  }
