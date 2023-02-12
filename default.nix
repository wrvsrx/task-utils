{ mkDerivation, aeson, base, bytestring, containers, fgl, graphviz
, lib, optparse-applicative, taskwarrior, text, time, uuid
}:
mkDerivation {
  pname = "taskwarrior-to-dot";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    aeson base bytestring containers fgl graphviz optparse-applicative
    taskwarrior text time uuid
  ];
  homepage = "https://github.com/wrvsrx/taskwarrior-to-dot";
  license = lib.licenses.mit;
  mainProgram = "taskwarrior-to-dot";
}
