let

  project = import ./nix/default.nix {};

in

  project.gen-tm.compontents.library
