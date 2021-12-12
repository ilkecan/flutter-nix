{
  description = "flutter-nix";

  inputs = {
    flake-utils.url = "github:numtide/flake-utils";
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-21.11";
    nix-filter.url = "github:ilkecan/nix-filter";
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
      inherit (inputs.flake-utils.lib)
        eachSystem
      ;

      supportedSystems = [
        "x86_64-linux"
      ];

      privateOverlay = final: prev:
        let
          inherit (prev)
            callPackage
          ;
        in
        {
          nix-filter = inputs.nix-filter.lib;
          flutter-nix = prev.flutter-nix // {
            internal = {
              sdk-dependencies =
                callPackage ./nix/flutter/sdk-dependencies.nix { };
            };
          };
        };
    in
    {
      inherit supportedSystems;

      overlay = final: prev:
        let
          pkgs = prev.appendOverlays [
            (import ./nix/haskell)
            inputs.android-nixpkgs.overlay
            privateOverlay
          ];

          inherit (pkgs)
            callPackage
            haskellPackages
          ;
        in
        {
          flutter-nix = {
            buildFlutterApp = callPackage ./nix/flutter/build-flutter-app { };
            mkShell = callPackage ./nix/flutter/mk-shell.nix { };
            translator = haskellPackages.flutter-nix-translator;
            supportedPlatforms = [
              "linux"
              "web"
            ];
          };
        };
    } // eachSystem supportedSystems (system:
      let
        pkgs = import nixpkgs {
          inherit system;
          overlays = [
            self.overlay
            privateOverlay
          ];
        };

        inherit (pkgs)
          callPackage
          flutter-nix
        ;
      in
      rec {
        packages = {
          inherit (flutter-nix)
            translator
          ;
        };
        defaultPackage = packages.translator;

        devShell = callPackage ./nix/dev-shell.nix { };
      }
    );
}
