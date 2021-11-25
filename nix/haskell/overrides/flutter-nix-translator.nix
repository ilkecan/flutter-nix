{
  flutter-nix,
  haskell,
  haskellPackagesPrev,
  makeWrapper,
  nix-filter,
  nix-prefetch,
  runCommandNoCC,
  symlinkJoin,
}:

let
  inherit (haskell.lib)
    overrideCabal
  ;

  inherit (nix-filter)
    inDirectory
  ;
in

overrideCabal haskellPackagesPrev.flutter-nix-translator {
  src = symlinkJoin {
    name = "flutter-nix-translator";
    paths = [
      (nix-filter {
        root = ./../../../translator;
        name = "flutter-nix-translator";
        include = [
          "flutter-nix-translator.cabal"
          (inDirectory "src")
        ];
      })
      (runCommandNoCC "fetch-pub" { } ''
        mkdir -p $out/data
        ln -s ${./../../flutter/fetch-pub.nix} $out/data/fetch-pub.nix
      '')
    ];
  };

  executableSystemDepends = [
    makeWrapper
    nix-prefetch
  ];

  postInstall = ''
    wrapProgram $out/bin/translator \
      --set FLUTTER_SDK_DEPENDENCIES_JSON "${flutter-nix.sdk-dependencies}" \
      --set PATH "${nix-prefetch}/bin"
  '';
}
