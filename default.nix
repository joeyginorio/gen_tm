let

  project = import ./nix/default.nix {};

  self = {
    library = project.gen-tm.components.library;
    exes = project.gen-tm.components.exes;
    tests = project.gen-tm.components.tests;
  };

in

  self
