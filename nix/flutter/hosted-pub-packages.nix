{
  callPackage,
  hostedPubPackages,
}:

let
  fetchPub = callPackage ./fetch-pub.nix { };
  fetch = {name, version, hash, url}: fetchPub {
    inherit name version url;
    sha256 = hash;
  };

  hostedPubPackageDrvs = map fetch hostedPubPackages;
in
hostedPubPackageDrvs
