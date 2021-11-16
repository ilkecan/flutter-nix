{
  description = "flutter";

  inputs = {
    # nix-filter.url = "github:numtide/nix-filter";
    flake-utils.url = "github:numtide/flake-utils";
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
  };

  outputs = { nixpkgs, ... }@inputs:
    let
      inherit (nixpkgs.lib)
        makeLibraryPath
        makeSearchPath
        subtractLists
      ;
      inherit (inputs.flake-utils.lib)
        defaultSystems
        eachSystem
      ;

      unsupportedSystems = [
        "aarch64-darwin"
        "aarch64-linux"
        "i686-linux"
        "x86_64-darwin"
      ];
      supportedSystems = subtractLists unsupportedSystems defaultSystems;
    in
    eachSystem supportedSystems (system:
      let
        config = {
          allowUnfree = true;
          android_sdk.accept_license = true;
        };

        pkgs = import nixpkgs {
          inherit system config;
        };

        inherit (pkgs)
          mkShell
        ;

        androidPkgs = import ./nix/android-sdk.nix pkgs;
      in
      {
        devShell = mkShell {
          packages = with pkgs; [
            dart
            flutter

            # flutter-linux
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

            # flutter-web
            chromium

            # flutter-android
            android-studio
            androidPkgs.platform-tools
          ];

          shellHook =
            let
              CPATH = with pkgs.xlibs; [
                libX11.dev # X11/Xlib.h
                xorgproto # X11/X.h
              ];

              LD_LIBRARY_PATH = with pkgs; [
                atk
                cairo
                epoxy.out
                gdk_pixbuf
                glib
                gtk3
                harfbuzz
                libglvnd
                pango
                wayland
                xlibs.libX11.out
              ];
            in
            ''
              export ANDROID_SDK_ROOT="${androidPkgs.androidsdk}/libexec/android-sdk"
              export CHROME_EXECUTABLE="${pkgs.chromium}/bin/chromium"
              export CPATH="${makeSearchPath "include" CPATH}"
              export LD_LIBRARY_PATH="${makeLibraryPath LD_LIBRARY_PATH}"

              flutter config --enable-linux-desktop
            '';
        };
      }
    );
}
