{
  description = "flutter-nix";

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
      inherit (inputs.flake-utils.lib)
        eachSystem
      ;

      supportedSystems = [
        "x86_64-linux"
      ];
    in
    {
      inherit supportedSystems;

      overlay = final: prev:
        let
          pkgs = final.appendOverlays [
            (_: _: { nix-filter = inputs.nix-filter.lib; })
            (import ./nix/haskell)
            inputs.android-nixpkgs.overlay
          ];

          inherit (pkgs)
            callPackage
            haskellPackages
          ;
        in
        {
          flutter-nix = {
            buildFlutterApp = callPackage ./nix/flutter/build-flutter-app.nix { };
            mkShell = callPackage ./nix/flutter/mk-shell.nix { };
            sdk-dependencies = callPackage ./nix/flutter/sdk-dependencies.nix { };
            translator = haskellPackages.flutter-nix-translator;
          };
        };
    } // eachSystem supportedSystems (system:
      let
        pkgs = import nixpkgs {
          inherit system;
          overlays = [
            self.overlay
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
