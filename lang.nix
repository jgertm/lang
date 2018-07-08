let
  config = {
    packageOverrides = pkgs: rec {
      haskellPackages = pkgs.haskellPackages.override {
        overrides = haskellPackagesNew: haskellPackagesOld: rec {
          fugue = haskellPackagesNew.callPackage ../fugue/default.nix { };
        };
      };
    };
  };

  pkgs = import <nixpkgs> { inherit config; };

in pkgs.haskellPackages.callPackage ./default.nix { }

