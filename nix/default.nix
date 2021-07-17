{ sources ? import ./sources.nix {}
, haskellNix ? import sources.haskellNix {}
, pkgs ? import
    haskellNix.sources.nixpkgs-unstable
    haskellNix.nixpkgsArgs
}:

with pkgs;

haskell-nix.project {

  src = haskell-nix.haskellLib.cleanGit {
    name = "gen-tm";
    src = ../.;
  };

  compiler-nix-name = "ghc8105";

}
