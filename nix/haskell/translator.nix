{ mkDerivation, aeson, base, containers, extra, lib
, optparse-applicative, process, yaml
}:
mkDerivation {
  pname = "translator";
  version = "0.1.0.0";
  src = ../.././translator;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    aeson base containers extra optparse-applicative process yaml
  ];
  license = lib.licenses.mpl20;
}
