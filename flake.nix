{
  description = "gen-tm";

  nixConfig = {
    substituters = [
      https://hydra.iohk.io
    ];
    trusted-public-keys = [
      hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ=
    ];
    bash-prompt = "\\[\\033[1m\\][dev-gen-tm]\\[\\033\[m\\]\\040\\w$\\040";
  };

  inputs = {
    nixpkgs.follows = "haskell-nix/nixpkgs-unstable";
    haskell-nix = {
      url = "github:input-output-hk/haskell.nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    utils.follows = "haskell-nix/flake-utils";
    iohkNix = {
      url = "github:input-output-hk/iohk-nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = { self, nixpkgs, haskell-nix, utils, iohkNix, ... }: with utils.lib;
    let
      inherit (nixpkgs) lib;
      inherit (lib);
      inherit (iohkNix.lib) collectExes;

      supportedSystems = ["x86_64-darwin" "x86_64-linux"];

      gitrev = self.rev or "dirty";

      overlays = [
        haskell-nix.overlay
        iohkNix.overlays.haskell-nix-extra

        (final: prev: {
          haskell-nix = prev.haskell-nix // {
            custom-tools = prev.haskell-nix.custom-tools // {
              haskell-language-server."1.3.0" = args':
                let
                  args = removeAttrs args' [ "version" ];
                in
                  (prev.haskell-nix.cabalProject (args // {
                    name = "haskell-language-server";
                    src = prev.fetchFromGitHub {
                      owner = "haskell";
                      repo = "haskell-language-server";
                      rev = "d7a745e9b5ae76a4bf4ee79a9fdf41cf6f1662bf";
                      sha256 = "0rxnkijdvglhamqfn8krsnnpj3s7kz2v5n5ndy37a41l161jqczx";
                    };
                  })).haskell-language-server.components.exes.haskell-language-server;
            };
          };
        })

        (final: prev: {
          inherit gitrev;
          commonLib = lib
            // iohkNix.lib;
        })

        (final: prev: {
          gen-tm-project = prev.haskell-nix.project {

            src = prev.haskell-nix.haskellLib.cleanGit {
              name = "gen-tm";
              src = ../.;
            };

            compiler-nix-name = "ghc8105";

            modules = [
              {
                packages.gen-tm.enableExecutableProfiling = true;
                enableLibraryProfiling = true;
              }
            ];
          };
        })
      ];
    
    in eachSystem supportedSystems (system:
      let
        pkgs = import nixpkgs { inherit system overlays; };

        legacyPkgs = haskell-nix.legacyPackages.${system}.appendOverlays overlays;

        inherit (pkgs.commonLib) eachEnv environments;

        devShell =  import ./shell.nix {
          inherit pkgs;
        };

        flake = pkgs.gen-tm-project.flake {};

        checks = collectChecks flake.packages;

        exes = collectExes flake.packages;

      in lib.recursiveUpdate flake {
        inherit environments checks legacyPkgs;

        defaultPackage = flake.packages."gen-tm:exe:gen-tm";

        inherit devShell;
      }
    );
}