final: prev:

let
  inherit (builtins)
    listToAttrs
    replaceStrings
    readDir
  ;

  inherit (lib)
    composeManyExtensions
    fold
    mapAttrs'
  ;

  inherit (final)
    lib
    nix-filter
    runCommandNoCC
    symlinkJoin
  ;

  inherit (nix-filter)
    inDirectory
  ;

  inherit (final.haskell.lib)
    doJailbreak
    dontCheck
    dontHaddock
    packagesFromDirectory
  ;

  # Disable tests for these packages
  dontCheckPackages = [
  ];

  # Jailbreak these packages
  # https://github.com/NixOS/jailbreak-cabal
  doJailbreakPackages = [
  ];

  # Disable haddocks for these packages
  dontHaddockPackages = [
  ];

  makeOverrides = function: names: haskellPackagesFinal: _:
    let
      toPackage = name: {
        inherit name;
        value = function haskellPackagesFinal.${name};
      };
    in
    listToAttrs (map toPackage names);

  # More exotic overrides go here
  manualOverrides = _: haskellPackagesPrev: with final.haskell.lib; {
    translator = overrideCabal haskellPackagesPrev.translator {
      src = symlinkJoin {
        name = "translator";
        paths = [
          (nix-filter {
            root = ./../translator;
            name = "translator";
            include = [
              "translator.cabal"
              (inDirectory "src")
            ];
          })
          (runCommandNoCC "fetch-pub" {} ''
            mkdir -p $out/data
            ln -s ${./fetch-pub.nix} $out/data/fetch-pub.nix
          '')
        ];
      };
    };
  };
in
{
  haskellPackages = prev.haskellPackages.override {
    overrides = composeManyExtensions [
      (packagesFromDirectory { directory = ./haskell; })
      (makeOverrides dontCheck dontCheckPackages)
      (makeOverrides doJailbreak doJailbreakPackages)
      (makeOverrides dontHaddock dontHaddockPackages)
      manualOverrides
    ];
  };
}
