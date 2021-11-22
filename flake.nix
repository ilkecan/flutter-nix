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
      inherit (inputs.flake-utils.lib)
        eachSystem
      ;

      supportedSystems = [
        "x86_64-linux"
      ];
    in
    {
      overlay = final: prev: {
        buildFlutterApp = import ./nix/flutter-linux.nix final;
      };
    } // eachSystem supportedSystems (system:
      let
        pkgs = import nixpkgs {
          inherit system;
          overlays = [
            inputs.android-nixpkgs.overlay
            (import ./nix/haskell.nix)
            (final: prev: { nix-filter = inputs.nix-filter.lib; })
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

        devShell = import ./nix/devshell.nix pkgs;
      }
    );
}
