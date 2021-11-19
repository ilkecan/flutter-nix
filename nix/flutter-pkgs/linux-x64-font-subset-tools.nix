{
  fetchzip ? (import <nixpkgs> {}).fetchzip,
  ...
}:

fetchzip {
  name = "linux-x64-font-subset-tools";
  url = "https://storage.googleapis.com/flutter_infra_release/flutter/b3af521a050e6ef076778bcaf16e27b2521df8f8/linux-x64/font-subset.zip";
  sha256 = "sha256-cFDq/sO4Ot6Psb3QrCh+22zMWjxOhkIIjbnoalTOxYM=";
  stripRoot = false;

  passthru = {
    stamp = "b3af521a050e6ef076778bcaf16e27b2521df8f8";
  };
}
