{ stdenv, haskellPackages, makeWrapper, lib }:
stdenv.mkDerivation rec {
  name = "taskwarrior-to-dot";
  buildInputs = [
    (haskellPackages.ghcWithPackages (ps: with ps; [
      graphviz
      aeson
      optparse-applicative
      taskwarrior
    ]))
  ];
  src = ./.;
  buildPhase = ''
    ghc Main
  '';
  installPhase = ''
    mkdir -p $out/bin
    cp Main $out/bin/${name}
  '';
}
