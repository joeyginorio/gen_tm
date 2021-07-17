let

  project = import ./nix/default.nix {};

in

  project.gen-tm.components.library
