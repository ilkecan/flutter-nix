{
  flutter-nix,
  haskellPackages,
  reuse,
  shellcheck,
  statix,
}:

let
  haskellTools = with haskellPackages; [
    cabal-install
    cabal2nix
    ghcid
    hlint
    nix-linter
    ormolu
    scan
    stan
    weeder
  ];
in

haskellPackages.shellFor {
  packages = _: [ flutter-nix.translator ];
  buildInputs = [
    reuse
    shellcheck
    statix
  ] ++ haskellTools;

  shellHook = ''
    export PATH="$PWD/scripts:$PATH"
    export FLUTTER_SDK_DEPENDENCIES_JSON=${flutter-nix.internal.sdk-dependencies}
  '';
}
