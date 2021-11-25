{
  callPackage,
  fetchzip,
  flutter,
  lib,
  makeWrapper,
  nix-filter,
  stdenv,
}:

{
  src,
  name,
  version,
  filterSrc ? true,
  flutterNixLockFile ? src + "/flutter-nix-lock.json",
}:

let
  inherit (builtins)
    mapAttrs
  ;

  inherit (lib)
    importJSON
  ;

  inherit (nix-filter)
    inDirectory
  ;

  inherit (importJSON flutterNixLockFile)
    pubPackages
    sdkDependencies
  ;

  hostedPubPackageDrvs =
    callPackage ./hosted-pub-packages.nix { hostedPubPackages = pubPackages.hosted; };
  packageConfigJson = callPackage ./package-config-json.nix {
    inherit hostedPubPackageDrvs;
    sdkPubPackages = pubPackages.sdk;
  };
  sdkDepDrvs = mapAttrs (_: fetchzip) sdkDependencies;

  inherit (sdkDepDrvs)
    flutter_patched_sdk
    flutter_patched_sdk_product
    gradle_wrapper
    linux-x64-artifacts
    linux-x64-font-subset
    linux-x64-linux-x64-flutter-gtk
    linux-x64-profile-linux-x64-flutter-gtk
    linux-x64-release-linux-x64-flutter-gtk
    material_fonts
    sky_engine
  ;

  linux = callPackage ./platforms/linux.nix { };

  createCacheStamp = { name, from ? name }: ''
    ln -s \
      "${flutter.unwrapped}/bin/internal/${from}.version" \
      "${name}.stamp"
  '';
in

stdenv.mkDerivation {
  inherit name version;

  src =
    if filterSrc
    then nix-filter {
      root = src;
      inherit name;
      include = [
        "flutter-nix-lock.json"
        "pubspec.yaml"
        (inDirectory "lib")
        (inDirectory "linux")
      ];
    }
    else src;

  nativeBuildInputs = [
    flutter

    makeWrapper
  ]
  ++ hostedPubPackageDrvs
  ++ linux.packages;

  dontUseCmakeConfigure = true;

  preBuild = ''
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

    ln -s ${linux-x64-linux-x64-flutter-gtk}/* artifacts/engine/linux-x64
    ln -s ${linux-x64-profile-linux-x64-flutter-gtk} artifacts/engine/linux-x64-profile
    ln -s ${linux-x64-release-linux-x64-flutter-gtk} artifacts/engine/linux-x64-release

    ${createCacheStamp { name = "flutter_sdk"; from = "engine"; }}
    ${createCacheStamp { name = "font-subset"; from = "engine"; }}
    ${createCacheStamp { name = "gradle_wrapper"; }}
    ${createCacheStamp { name = "linux-sdk"; from = "engine"; }}
    ${createCacheStamp { name = "material_fonts"; }}

    popd

    install -D ${packageConfigJson} .dart_tool/package_config.json
  ''
  + linux.shellHook;

  buildPhase = ''
    runHook preBuild

    flutter build linux --no-pub

    runHook postBuild
  '';

  installPhase = ''
    runHook preInstall

    mkdir -p $out/bin/
    mv build/linux/x64/release/bundle/ $out/bundle

    makeWrapper "$out/bundle/friendly_chat" "$out/bin/friendly_chat" \
      --set LD_LIBRARY_PATH "$LD_LIBRARY_PATH"

    runHook postInstall
  '';

  passthru = {
    inherit
      hostedPubPackageDrvs
      packageConfigJson
      sdkDepDrvs
    ;
  };
}
