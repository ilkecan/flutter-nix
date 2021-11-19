{
  description = "flutter";

  inputs = {
    flake-utils.url = "github:numtide/flake-utils";
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
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
          ];
        };
      in
      {
        devShell = import ./nix/devshell.nix pkgs;
      }
    );
}
