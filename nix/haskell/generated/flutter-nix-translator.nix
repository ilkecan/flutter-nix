{ mkDerivation, aeson, aeson-pretty, async, base, bytestring
, co-log, containers, extra, lib, optparse-applicative, process
, text, transformers, unordered-containers, vector, yaml
}:
mkDerivation {
  pname = "flutter-nix-translator";
  version = "0.1.0.0";
  src = ../../.././translator;
  isLibrary = false;
  isExecutable = true;
  enableSeparateDataOutput = true;
  executableHaskellDepends = [
    aeson aeson-pretty async base bytestring co-log containers extra
    optparse-applicative process text transformers unordered-containers
    vector yaml
  ];
  description = "flutter-nix translator";
  license = lib.licenses.mpl20;
}
