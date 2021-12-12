{
  callPackage,
  fetchzip,
  flutter,
  flutter-nix,
  lib,
  nix-filter,
  stdenv,
}:

{
  src,
  platform,
  name ? null,
  version ? null,
  flutterNixLockFile ? src + "/flutter-nix-lock.json",
  filterSrc ? true,
}@args:

let
  inherit (builtins)
    mapAttrs
  ;

  inherit (lib)
    assertOneOf
    importJSON
    optional
    optionalString
    optionals
  ;

  inherit (nix-filter)
    inDirectory
  ;

  flutterNixLock = importJSON flutterNixLockFile;
  inherit (flutterNixLock)
    pubPackages
    sdkDependencies
  ;

  hostedPubPackageDrvs =
    callPackage ./../hosted-pub-packages.nix { hostedPubPackages = pubPackages.hosted; };
  packageConfigJson = callPackage ./../package-config-json.nix {
    inherit hostedPubPackageDrvs;
    sdkPubPackages = pubPackages.sdk;
  };
  sdkDepDrvs = mapAttrs (_: mapAttrs (_: fetchzip)) sdkDependencies;

  inherit (sdkDepDrvs.common)
    flutter_patched_sdk
    flutter_patched_sdk_product
    gradle_wrapper
    linux-x64-artifacts
    linux-x64-font-subset
    material_fonts
    sky_engine
  ;

  inherit (callPackage ./lib.nix { })
    createCacheStamp
  ;

  pname = args.name or flutterNixLock.name;
in

assert (assertOneOf "platform" platform flutter-nix.supportedPlatforms);
(callPackage ./${platform}.nix {
  sdkDependencies = sdkDepDrvs.${platform};
}) {
  inherit pname;
  version = args.version or flutterNixLock.version;

  src =
    if filterSrc
    then nix-filter {
      root = src;
      name = pname;
      include = [
        "flutter-nix-lock.json"
        "pubspec.yaml"
        (inDirectory "lib")
        (inDirectory platform)
      ];
    }
    else src;

  nativeBuildInputs =
    [
      flutter
    ]
    ++ hostedPubPackageDrvs
  ;

  dontUseCmakeConfigure = true;

  preBuild = ''
    install -D ${packageConfigJson} .dart_tool/package_config.json

    HOME=$(mktemp -d)
    mkdir -p "$HOME"/.cache/flutter/{artifacts,pkg}
    mkdir -p "$HOME"/.cache/flutter/artifacts/engine/{common,linux-x64}

    pushd "$HOME/.cache/flutter"

    ln -s ${material_fonts} artifacts/material_fonts

    ln -s ${gradle_wrapper} artifacts/gradle_wrapper

    ln -s ${sky_engine} pkg/sky_engine
    ln -s ${flutter_patched_sdk} artifacts/engine/common/flutter_patched_sdk
    ln -s ${flutter_patched_sdk_product} artifacts/engine/common/flutter_patched_sdk_product
    ln -s ${linux-x64-artifacts}/* artifacts/engine/linux-x64

    ln -s ${linux-x64-font-subset}/* artifacts/engine/linux-x64

    ${createCacheStamp { name = "flutter_sdk"; from = "engine"; }}
    ${createCacheStamp { name = "font-subset"; from = "engine"; }}
    ${createCacheStamp { name = "gradle_wrapper"; }}
    ${createCacheStamp { name = "material_fonts"; }}

    popd
  '';
}
