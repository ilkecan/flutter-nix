{
  callPackage,
  hostedPackages,
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

  hostedPubDeps = writeText "hosted-pub-deps.nix" ''
    fetchPub:
    [
    ${concatMapStrings fetch hostedPackages}
    ]
  '';
in
import hostedPubDeps fetchPub
