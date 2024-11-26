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
            packages.default = pkgs.haskellPackages.callPackage ./default.nix {
              doclayout = (
                pkgs.haskell.lib.overrideSrc pkgs.haskellPackages.doclayout {
                  src = pkgs.fetchFromGitHub {
                    owner = "jgm";
                    repo = "doclayout";
                    rev = "0.5";
                    hash = "sha256-gTJhoM0WEF+5sbA3bEH+eYAjixNQf1oi2WbcBJpwLZg=";
                  };
                  version = "0.5";

                }
              );
            };
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
