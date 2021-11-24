{
  description = "flutter";

  inputs = {
    flake-utils.url = "github:numtide/flake-utils";
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    nix-filter.url = "github:numtide/nix-filter";
    android-nixpkgs = {
      url = "github:tadfisher/android-nixpkgs/stable";
      inputs = {
        nixpkgs.follows = "nixpkgs";
        flake-utils.follows = "flake-utils";
      };
    };
  };

  outputs = { self, nixpkgs, ... }@inputs:
    let
      inherit (nixpkgs.lib)
        composeManyExtensions
      ;

      inherit (inputs.flake-utils.lib)
        eachSystem
      ;

      nixFilterOverlay = _: _: {
        nix-filter = inputs.nix-filter.lib;
      };

      supportedSystems = [
        "x86_64-linux"
      ];
    in
    {
      inherit supportedSystems;

      overlay = composeManyExtensions [
        (final: _:
          let
            pkgs = final.appendOverlays [
              inputs.android-nixpkgs.overlay
              nixFilterOverlay
            ];

            inherit (pkgs)
              callPackage
            ;
          in
          {
            flutter-nix = {
              mkShell = import ./nix/mk-shell.nix pkgs;
              buildFlutterApp = import ./nix/flutter-linux.nix pkgs;
              sdk-dependencies =
                callPackage ./nix/flutter-sdk-dependencies.nix { };
            };
          }
        )
      ];
    } // eachSystem supportedSystems (system:
      let
        pkgs = import nixpkgs {
          inherit system;
          overlays = [
            (import ./nix/haskell.nix)
            nixFilterOverlay
            self.overlay
          ];
        };

        inherit (pkgs)
          haskellPackages
          nix-prefetch
          writeShellScriptBin
          flutter-nix
        ;
      in
      rec {
        packages = {
          translator = writeShellScriptBin "translator" ''
            export FLUTTER_SDK_DEPENDENCIES_JSON=${flutter-nix.sdk-dependencies}
            export PATH="${nix-prefetch}/bin"
            exec ${haskellPackages.translator}/bin/translator "$@"
          '';
        };
        defaultPackage = packages.translator;

        devShell = import ./nix/dev-shell.nix pkgs;
      }
    );
}
