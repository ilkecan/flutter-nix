{
  at_spi2_core,
  atk,
  cairo,
  callPackage,
  clang,
  cmake,
  dbus,
  epoxy,
  gdk-pixbuf,
  glib,
  gnome,
  gnome2,
  gtk3,
  harfbuzz,
  lib,
  libdatrie,
  libselinux,
  libsepol,
  libthai,
  libuuid,
  libxkbcommon,
  ninja,
  pcre,
  pkg-config,
  xlibs,
}:

let
  inherit (lib)
    makeLibraryPath
    makeSearchPath
  ;

  inherit (callPackage ./. {})
    exportEnvVars
  ;

  envVars = {
    CPATH = with xlibs; makeSearchPath "include" [
      libX11.dev # X11/Xlib.h
      xorgproto # X11/X.h
    ];

    LD_LIBRARY_PATH = makeLibraryPath [
      atk.out
      cairo.out
      epoxy.out
      gdk-pixbuf.out
      glib.out
      gnome.gtk.out
      gnome2.pango.out
      harfbuzz.out
    ];
  };
in
{
  packages = [
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
  ];

  shellHook = ''
    flutter config --enable-linux-desktop > /dev/null
  '' + exportEnvVars envVars;
}
