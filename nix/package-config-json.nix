{
  flutter,
  hostedPubDeps,
  lib,
  sdkPackages,
  writeText,
}:

let
  inherit (builtins)
    toJSON
  ;

  inherit (lib)
    concatMapStringsSep
    getName
  ;

  inherit (lib.versions)
    majorMinor
  ;

  inherit (lib.strings)
    escapeNixString
  ;

  inherit (flutter.passthru)
    dart
  ;

  hostedConfig = drv:
    toJSON {
      name = getName drv.name;
      rootUri = "file://${drv}";
      packageUri = "lib/";
      languageVersion = majorMinor dart.version;
    };

  sdkConfig = { name }:
    toJSON {
      inherit name;
      rootUri = "file://${flutter.unwrapped}/${sdkConfigSuffix name}";
      packageUri = "lib/";
      languageVersion = majorMinor dart.version;
    };

  sdkConfigSuffix = name:
    {
      "flutter" = "packages/flutter";
      "flutter_test" = "packages/flutter_test";
      "sky_engine" = "bin/cache/pkg/sky_engine";
    }.${name};

  packageConfigJson = writeText "package-config.json" ''
    {
      "configVersion": 2,
      "packages": [
        ${concatMapStringsSep "," hostedConfig hostedPubDeps},
        ${concatMapStringsSep "," sdkConfig sdkPackages}
      ],
      "generated": "2021-11-17T00:19:07.676332Z",
      "generator": "pub",
      "generatorVersion": "${dart.version}"
    }
  '';
in
packageConfigJson
