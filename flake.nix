{
  description = "flake template";

  inputs = {
    nixpkgs.url = "github:wrvsrx/nixpkgs/patched-nixos-unstable";
    flake-parts = {
      url = "github:hercules-ci/flake-parts";
      inputs.nixpkgs-lib.follows = "nixpkgs";
    };
  };

  outputs =
    inputs:
    inputs.flake-parts.lib.mkFlake { inherit inputs; } (
      { withSystem, ... }:
      {
        systems = [ "x86_64-linux" ];
        perSystem =
          { pkgs, system, ... }:
          rec {
            _module.args.pkgs = import inputs.nixpkgs {
              inherit system;
              config.allowBroken = true;
            };
            packages.default = pkgs.haskellPackages.callPackage ./default.nix { };
            devShells.default = pkgs.haskellPackages.shellFor {
              packages = _: [
                packages.default
              ];
              nativeBuildInputs = with pkgs; [
                haskellPackages.cabal-fmt
                cabal2nix
                cabal-install
                haskell-language-server
              ];
            };
            formatter = pkgs.nixfmt-rfc-style;
          };
      }
    );
}
