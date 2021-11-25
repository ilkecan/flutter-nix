{
  callPackage,
  hostedPubPackages,
  lib,
  writeText,
}:

let
  inherit (lib)
    concatMapStrings
  ;

  inherit (lib.strings)
    escapeNixString
  ;

  fetchPub = callPackage ./fetch-pub.nix { };
  fetch = {name, version, hash, url}: ''
    (fetchPub {
      name = ${escapeNixString name};
      version = ${escapeNixString version};
      sha256 = ${escapeNixString hash};
      url = ${escapeNixString url};
    })
  '';

  hostedPubPackagesNix = writeText "hosted-pub-packages.nix" ''
    fetchPub:
    [
    ${concatMapStrings fetch hostedPubPackages}
    ]
  '';
in
import hostedPubPackagesNix fetchPub
