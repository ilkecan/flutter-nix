{
  lib,
  mkShell,
  ...
}@pkgs:

let
  inherit (lib)
    makeLibraryPath
    makeSearchPath
  ;

  androidSdk = import ./android-sdk.nix pkgs;

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
mkShell {
  packages = with pkgs; [
    dart
    flutter

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
  ];

  shellHook = ''
    export CHROME_EXECUTABLE="${pkgs.chromium}/bin/chromium"
    export CPATH="${makeSearchPath "include" CPATH}"
    export LD_LIBRARY_PATH="${makeLibraryPath LD_LIBRARY_PATH}"

    flutter config --enable-linux-desktop
  '';
}
