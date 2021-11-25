final: prev:

let
  inherit (builtins)
    attrNames
    listToAttrs
    readDir
    replaceStrings
  ;

  inherit (final)
    callPackage
    lib
  ;

  inherit (lib)
    composeManyExtensions
  ;

  inherit (final.haskell.lib)
    doJailbreak
    dontCheck
    packagesFromDirectory
  ;

  # Jailbreak these packages
  # https://github.com/NixOS/jailbreak-cabal
  doJailbreakPackages = [
  ];

  # Disable tests for these packages
  dontCheckPackages = [
  ];

  makeOverrides = function: names: _: haskellPackagesPrev:
    let
      toPackage = name: {
        inherit name;
        value = function haskellPackagesPrev.${name};
      };
    in
    listToAttrs (map toPackage names);

  overridesFromDirectory = directory:
    _: haskellPackagesPrev:
      let
        overridePaths = attrNames (readDir directory);

        toKeyVal = file: {
          name  = replaceStrings [ ".nix" ] [ "" ] file;
          value = callPackage (directory + "/${file}") { inherit haskellPackagesPrev; };
        };
      in
      listToAttrs (map toKeyVal overridePaths);
in
{
  haskellPackages = prev.haskellPackages.override {
    overrides = composeManyExtensions [
      (packagesFromDirectory { directory = ./generated; })
      (overridesFromDirectory ./overrides)
      (makeOverrides doJailbreak doJailbreakPackages)
      (makeOverrides dontCheck dontCheckPackages)
    ];
  };
}
