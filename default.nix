{ mkDerivation, aeson, base, bytestring, containers, fgl, graphviz
, lib, optparse-applicative, pretty-simple, process, taskwarrior
, text, time, unicode-show, utf8-string, uuid
}:
mkDerivation {
  pname = "task-utils";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    aeson base bytestring containers fgl graphviz optparse-applicative
    pretty-simple process taskwarrior text time unicode-show
    utf8-string uuid
  ];
  homepage = "https://github.com/wrvsrx/task-utils";
  license = lib.licenses.mit;
  mainProgram = "task-utils";
}
