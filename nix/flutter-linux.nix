{
  callPackage,
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
  inherit (lib)
    importJSON
    makeLibraryPath
    makeSearchPath
  ;

  inherit (nix-filter)
    inDirectory
  ;

  inherit (importJSON flutterNixLockFile)
    packages
  ;
  hostedPackages = packages.hosted;
  sdkPackages = packages.sdk;

  hostedPubDeps = callPackage ./hosted-pub-deps.nix { inherit hostedPackages; };
  packageConfigJson = callPackage ./package-config-json.nix {
    inherit
      hostedPubDeps
      sdkPackages
    ;
  };

  material_fonts = import ./flutter-pkgs/material-fonts.nix pkgs;
  gradle_wrapper = import ./flutter-pkgs/gradle-wrapper.nix pkgs;
  sky_engine = import ./flutter-pkgs/sky_engine.nix pkgs;
  flutter_patched_sdk = import ./flutter-pkgs/flutter_patched_sdk.nix pkgs;
  flutter_patched_sdk_product = import ./flutter-pkgs/flutter_patched_sdk_product.nix pkgs;
  linux-x64_tools = import ./flutter-pkgs/linux-x64-tools.nix pkgs;
  font-subset = import ./flutter-pkgs/linux-x64-font-subset-tools.nix pkgs;
  linux-sdk = import ./flutter-pkgs/linux-x64-flutter-gtk-tools.nix pkgs;
  linux-sdk-profile = import ./flutter-pkgs/linux-x64-profile-flutter-gtk-tools.nix pkgs;
  linux-sdk-release = import ./flutter-pkgs/linux-x64-release-flutter-gtk-tools.nix pkgs;
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
        (inDirectory "linux")
        (inDirectory "lib")
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
    echo ${material_fonts.stamp} > material_fonts.stamp

    ln -s ${gradle_wrapper} artifacts/gradle_wrapper
    echo ${gradle_wrapper.stamp} > gradle_wrapper.stamp

    ln -s ${sky_engine} pkg/sky_engine
    ln -s ${flutter_patched_sdk} artifacts/engine/common/flutter_patched_sdk
    ln -s ${flutter_patched_sdk_product} artifacts/engine/common/flutter_patched_sdk_product
    ln -s ${linux-x64_tools}/* artifacts/engine/linux-x64
    echo ${flutter_patched_sdk.stamp} > flutter_sdk.stamp

    ln -s ${font-subset}/* artifacts/engine/linux-x64
    echo ${font-subset.stamp} > font-subset.stamp

    ln -s ${linux-sdk}/* artifacts/engine/linux-x64
    ln -s ${linux-sdk-profile} artifacts/engine/linux-x64-profile
    ln -s ${linux-sdk-release} artifacts/engine/linux-x64-release
    echo ${linux-sdk.stamp} > linux-sdk.stamp

    popd

    flutter config --enable-linux-desktop

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
    inherit packageConfigJson;
  };
}
