let
  bootstrap = import <nixpkgs> { };

  nixpkgs = builtins.fromJSON (builtins.readFile ./nixos-18-09.json);

  src = bootstrap.fetchFromGitHub {
    owner = "NixOS";
    repo  = "nixpkgs";
    inherit (nixpkgs) rev sha256;
  };

  config = {
    packageOverrides = pkgs: rec {
      haskellPackages = pkgs.haskellPackages.override {
        overrides = haskellPackagesNew: haskellPackagesOld: rec {
          generic-lens = haskellPackagesNew.callPackage ./generic-lens.nix { };
        };
      };
    };
  };

  pkgs = import src { inherit config; };

in pkgs.haskellPackages.callPackage ./default.nix { }
