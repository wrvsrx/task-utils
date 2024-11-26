{ mkDerivation, aeson, base, bytestring, Chart, Chart-cairo
, containers, data-default, deepseq, directory, doclayout, fgl
, filepath, graphviz, iCalendar, lib, optparse-applicative, parsec
, pretty-simple, process, split, taskwarrior, terminal-size, text
, time, transformers, unicode-show, utf8-string, uuid
}:
mkDerivation {
  pname = "task-utils";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson base bytestring Chart Chart-cairo containers data-default
    deepseq directory doclayout fgl filepath graphviz iCalendar
    optparse-applicative parsec pretty-simple process split taskwarrior
    terminal-size text time transformers unicode-show utf8-string uuid
  ];
  executableHaskellDepends = [
    aeson base bytestring containers doclayout fgl graphviz
    optparse-applicative parsec pretty-simple process taskwarrior
    terminal-size text time unicode-show utf8-string uuid
  ];
  doHaddock = false;
  homepage = "https://github.com/wrvsrx/task-utils";
  license = lib.licenses.mit;
  mainProgram = "task-utils";
}
