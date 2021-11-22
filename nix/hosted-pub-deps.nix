{
  callPackage,
  flutterNixLockFile,
  lib,
  writeText,
}:

let
  inherit (lib)
    concatMapStrings
    importJSON
  ;

  inherit (lib.strings)
    escapeNixString
  ;

  inherit (importJSON flutterNixLockFile)
    packages
  ;
  hostedPackages = packages.hosted;
  sdkPackages = packages.sdk;

  fetchPub = callPackage ./fetch-pub.nix { };
  fetch = {name, version, hash, url}: ''
    (fetchPub {
      name = ${escapeNixString name};
      version = ${escapeNixString version};
      sha256 = ${escapeNixString hash};
      url = ${escapeNixString url};
    })
  '';

  hostedPubDeps = writeText "hosted-pub-deps.nix" ''
    fetchPub:
    [
    ${concatMapStrings fetch hostedPackages}
    ]
  '';
in
import hostedPubDeps fetchPub
