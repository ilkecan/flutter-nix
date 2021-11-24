{
  callPackage,
  fetchzip,
  lib,
  nix-filter,
  stdenv,
  ...
}@pkgs:

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
    makeLibraryPath
    makeSearchPath
  ;

  inherit (nix-filter)
    inDirectory
  ;

  flutterNixLock = importJSON flutterNixLockFile;
  inherit (flutterNixLock)
    pubPackages
  ;

  hostedPackages = pubPackages.hosted;
  sdkPackages = pubPackages.sdk;
  sdkDependencies = mapAttrs (_: fetchzip) flutterNixLock.sdkDependencies;

  hostedPubDeps = callPackage ./hosted-pub-deps.nix { inherit hostedPackages; };
  packageConfigJson = callPackage ./package-config-json.nix {
    inherit
      hostedPubDeps
      sdkPackages
    ;
  };

  inherit (sdkDependencies)
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

  createCacheStamp = { name, from ? name }: ''
    ln -s \
      "${pkgs.flutter.unwrapped}/bin/internal/${from}.version" \
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

  CPATH = with pkgs.xlibs; makeSearchPath "include" [
    libX11.dev # X11/Xlib.h
    xorgproto # X11/X.h
  ];

  LD_LIBRARY_PATH = with pkgs; makeLibraryPath [
    atk.out
    cairo.out
    epoxy.out
    gdk-pixbuf.out
    glib.out
    gnome.gtk.out
    gnome2.pango.out
    harfbuzz.out
  ];

  nativeBuildInputs = with pkgs; [
    flutter

    at_spi2_core.dev # atspi-2.pc
    clang
    cmake
    dbus.dev # dbus-1.pc
    epoxy.dev # epoxy.pc
    gtk3.dev
    libdatrie.dev # libdatrie.pc
    libselinux.dev # libselinux.pc
    libsepol.dev # libsepol.pc
    libthai.dev # libthai.pc
    libuuid.dev # mount.pc
    libxkbcommon.dev # xkbcommon.pc
    ninja
    pcre.dev # libpcre.pc
    pkg-config
    xlibs.libXdmcp.dev # xdmcp.pc
    xlibs.libXtst.out # xtst.pc

    makeWrapper
  ] ++ hostedPubDeps;

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

    flutter config --enable-linux-desktop > /dev/null

    install -D ${packageConfigJson} .dart_tool/package_config.json
  '';

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
      hostedPubDeps
      packageConfigJson
      sdkDependencies
    ;
  };
}
