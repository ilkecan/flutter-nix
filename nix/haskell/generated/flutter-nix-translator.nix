{ mkDerivation, aeson, async, base, containers, extra, lib
, optparse-applicative, process, text, transformers, yaml
}:
mkDerivation {
  pname = "flutter-nix-translator";
  version = "0.1.0.0";
  src = ../../.././translator;
  isLibrary = false;
  isExecutable = true;
  enableSeparateDataOutput = true;
  executableHaskellDepends = [
    aeson async base containers extra optparse-applicative process text
    transformers yaml
  ];
  description = "flutter-nix translator";
  license = lib.licenses.mpl20;
}
