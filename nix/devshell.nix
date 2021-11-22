{
  haskellPackages,
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
    export FETCH_PUB_NIX="$PWD/nix/fetch-pub.nix"
    export PATH="$PWD/scripts:$PATH"
  '';
}
