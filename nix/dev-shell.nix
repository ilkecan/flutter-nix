{
  haskellPackages,
  flutter-nix,
  ...
}@pkgs:

haskellPackages.shellFor {
  packages = p: with p; [ translator ];
  buildInputs = with pkgs; with haskellPackages; [
    cabal-install
    cabal2nix
    ghcid
    hlint
    nix-linter
    nix-prefetch
    ormolu
    reuse
    scan
    shellcheck
    stan
    statix
    weeder
  ];

  shellHook = ''
    export PATH="$PWD/scripts:$PATH"
    export FLUTTER_SDK_DEPENDENCIES_JSON=${flutter-nix.sdk-dependencies}
  '';
}
