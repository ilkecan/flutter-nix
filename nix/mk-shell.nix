{
  callPackage,
  lib,
  mkShell,
  flutter-nix,
  ...
}@pkgs:

{
  packages ? [ ],
  shellHook ? "",
  ...
}@args:

let
  inherit (lib)
    makeLibraryPath
    makeSearchPath
  ;

  androidSdk = callPackage ./android-sdk.nix { };

  CPATH = with pkgs.xlibs; [
    libX11.dev # X11/Xlib.h
    xorgproto # X11/X.h
  ];

  LD_LIBRARY_PATH = with pkgs; [
    atk.out
    cairo.out
    epoxy.out
    gdk-pixbuf.out
    glib.out
    gnome.gtk.out
    gnome2.pango.out
    harfbuzz.out
  ];
in
mkShell (args // {
  packages = with pkgs; [
    flutter
    flutter.dart

    # flutter-linux
    at_spi2_core.dev # atspi-2.pc
    clang
    cmake
    dbus.dev # dbus-1.pc
    epoxy.dev # epoxy.pc
    gtk3
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

    # flutter-web
    chromium

    # flutter-android
    androidSdk
  ] ++ packages;

  shellHook = ''
    export CHROME_EXECUTABLE="${pkgs.chromium}/bin/chromium"
    export CPATH="${makeSearchPath "include" CPATH}"
    export LD_LIBRARY_PATH="${makeLibraryPath LD_LIBRARY_PATH}"
    export FLUTTER_SDK_DEPENDENCIES_JSON=${flutter-nix.sdk-dependencies}

    # TODO: check an argument before doing this
    flutter config --enable-linux-desktop > /dev/null
  '' + shellHook;
})
