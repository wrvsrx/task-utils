{ mkDerivation, aeson, base, bytestring, containers, doclayout, fgl
, graphviz, lib, optparse-applicative, parsec, pretty-simple
, process, taskwarrior, terminal-size, text, time, unicode-show
, utf8-string, uuid
}:
mkDerivation {
  pname = "task-utils";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    aeson base bytestring containers doclayout fgl graphviz
    optparse-applicative parsec pretty-simple process taskwarrior
    terminal-size text time unicode-show utf8-string uuid
  ];
  homepage = "https://github.com/wrvsrx/task-utils";
  license = lib.licenses.mit;
  mainProgram = "task-utils";
}
