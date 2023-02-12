{ mkDerivation, aeson, base, graphviz, lib, optparse-applicative
, taskwarrior
}:
mkDerivation {
  pname = "taskwarrior-to-dot";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    aeson base graphviz optparse-applicative taskwarrior
  ];
  homepage = "https://github.com/wrvsrx/taskwarrior-to-dot";
  license = lib.licenses.mit;
  mainProgram = "taskwarrior-to-dot";
}
