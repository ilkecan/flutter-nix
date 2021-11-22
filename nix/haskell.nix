final: prev:

let
  inherit (builtins)
    listToAttrs
    replaceStrings
    readDir
  ;

  inherit (final)
    lib
    nix-filter
    runCommandNoCC
    symlinkJoin
  ;

  inherit (lib)
    mapAttrs'
    fold
    composeExtensions
  ;

  inherit (nix-filter)
    inDirectory
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

  makeOverrides = function: names: haskellPackagesFinal: haskellPackagesPrev:
    let
      toPackage = name: {
        inherit name;

        value = function haskellPackagesPrev.${name};
      };
    in
    listToAttrs (map toPackage names);

  composeExtensionsList = fold composeExtensions (_: _: { });

  # More exotic overrides go here
  manualOverrides = haskellPackagesFinal: haskellPackagesPrev: with final.haskell.lib; {
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
    overrides = with final.haskell.lib; composeExtensionsList [
      (packagesFromDirectory { directory = ./haskell; })
      (makeOverrides dontCheck dontCheckPackages)
      (makeOverrides doJailbreak doJailbreakPackages)
      (makeOverrides dontHaddock dontHaddockPackages)
      manualOverrides
    ];
  };
}
