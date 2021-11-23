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

  outputs = { nixpkgs, ... }@inputs:
    let
      inherit (nixpkgs.lib)
        composeManyExtensions
      ;

      inherit (inputs.flake-utils.lib)
        eachSystem
      ;

      nixFilterOverlay = final: prev: {
        nix-filter = inputs.nix-filter.lib;
      };

      supportedSystems = [
        "x86_64-linux"
      ];
    in
    {
      inherit supportedSystems;

      overlay = composeManyExtensions [
        (final: prev:
          let
            pkgs = final.appendOverlays [
              inputs.android-nixpkgs.overlay
              nixFilterOverlay
            ];
          in
          {
            flutter-nix = {
              mkShell = import ./nix/mk-shell.nix pkgs;
              buildFlutterApp = import ./nix/flutter-linux.nix pkgs;
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
          ];
        };

        inherit (pkgs)
          haskellPackages
          nix-prefetch
          writeShellScriptBin
        ;
      in
      rec {
        packages = {
          translator = writeShellScriptBin "translator" ''
            export PATH="${nix-prefetch}/bin"
            exec ${haskellPackages.translator}/bin/translator "$@"
          '';
        };
        defaultPackage = packages.translator;

        devShell = import ./nix/dev-shell.nix pkgs;
      }
    );
}
