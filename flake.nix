{
  description = "flake template";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let pkgs = import nixpkgs { inherit system; config.allowUnfree = true; };
      in rec {
        packages.default = pkgs.haskellPackages.callPackage ./default.nix { };
        devShells.default = pkgs.mkShell { inputsFrom = [ packages.default ]; };
        formatters.default = pkgs.nixpkgs-fmt;
      }
    );
}
