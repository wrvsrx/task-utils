{
  description = "flake template";

  inputs = {
    flake-lock.url = "github:wrvsrx/flake-lock";
    nixpkgs.follows = "flake-lock/nixpkgs";
    flake-parts.follows = "flake-lock/flake-parts";
  };

  outputs =
    inputs:
    inputs.flake-parts.lib.mkFlake { inherit inputs; } (
      { withSystem, ... }:
      {
        systems = [ "x86_64-linux" ];
        perSystem =
          { pkgs, ... }:
          rec {
            packages.default = pkgs.haskellPackages.callPackage ./default.nix { };
            devShells.default = pkgs.mkShell {
              inputsFrom = [ packages.default.env ];
              nativeBuildInputs = with pkgs; [
                haskellPackages.cabal-fmt
                cabal2nix
                cabal-install
                haskell-language-server
              ];
            };
            formatter = pkgs.nixpkgs-fmt;
          };
      }
    );
}
