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

  outputs = { self, nixpkgs, haskell-nix, utils, iohkNix, ... }:
    let
      inherit (nixpkgs) lib;
      inherit (lib) mapAttrs getAttrs attrNames;
      inherit (utils.lib) eachSystem;
      inherit (iohkNix.lib) prefixNamesWith collectExes;

      supportedSystems = ["x86_64-linux" "x86_64-darwin"];

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
                    sha256map = {
                      "https://github.com/hsyl20/ghc-api-compat"."8fee87eac97a538dbe81ff1ab18cff10f2f9fa15" = "16bibb7f3s2sxdvdy2mq6w1nj1lc8zhms54lwmj17ijhvjys29vg";
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
          gen-tm-project = 
            let
              src = final.haskell-nix.haskellLib.cleanGit {
                name = "gen-tm";
                src = ./.;
              };
              compiler-nix-name = "ghc8105";
              projectPackages = lib.attrNames (final.haskell-nix.haskellLib.selectProjectPackages
                (final.haskell-nix.cabalProject' {
                  inherit src compiler-nix-name;
                }).hsPkgs);
            in
              final.haskell-nix.cabalProject' {
                inherit src compiler-nix-name;

                modules = [
                  {
                    packages.gen-tm.enableExecutableProfiling = true;
                    enableLibraryProfiling = true;

                    # packages.gen-tm.components.exes.gen-tm.configureFlags =
                    #   lib.optionals final.stdenv.hostPlatform.isMusl [
                    #     "--disable-executable-dynamic"
                    #     "--disable-shared"
                    #     "--ghc-option=-optl=-pthread"
                    #     "--ghc-option=-optl=-static"
                    #     "--ghc-option=-optl=-L${final.gmp6.override { withStatic = true; }}/lib"
                    #     "--ghc-option=-optl=-L${final.zlib.override { static = true; }}/lib"
                    #   ];
                  }

                  (lib.optionalAttrs final.stdenv.hostPlatform.isMusl (let
                    fullyStaticOptions = {
                      enableShared = false;
                      enableStatic = true;
                    };
                  in
                    {
                      packages = lib.genAttrs projectPackages (name: fullyStaticOptions);
                      doHaddock = false;
                    }
                  ))
                ];
              };
        })
      ];
    
    in eachSystem supportedSystems (system:
      let
        pkgs = import nixpkgs { inherit system overlays; };

        inherit (pkgs.commonLib) eachEnv environments;

        devShell =  import ./shell.nix {
          inherit pkgs;
        };

        flake = pkgs.gen-tm-project.flake {};

        staticFlake = pkgs.pkgsStatic.gen-tm-project.flake {};

        exes = collectExes flake.packages;
        exeNames = attrNames exes;
        lazyCollectExe = p: getAttrs exeNames (collectExes p);

        packages = {
          inherit (pkgs) gen-tm;
        }
        // exes
        // (prefixNamesWith "static/"
              (mapAttrs pkgs.rewriteStatic (lazyCollectExe staticFlake.packages)));

      in lib.recursiveUpdate flake {
        inherit environments packages;

        defaultPackage = flake.packages."gen-tm:exe:gen-tm";

        defaultApp = utils.lib.mkApp { drv = flake.packages."gen-tm:exe:gen-tm"; };

        inherit devShell;
      }
    );
}