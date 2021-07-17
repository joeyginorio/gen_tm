let

  project = import ./default.nix;

in
  project.shellFor {
    packages = ps: with ps; [];

    withHoogle = true;

    tools = {
      cabal = "latest";
      hlint = "latest";
      haskell-language-server = "latest";
    };

    buildInputs = [ (import <nixpkgs> {}).git ];

    exactDeps = true;
  }
