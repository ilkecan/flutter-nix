{
  fetchzip ? (import <nixpkgs> {}).fetchzip,
  ...
}:

fetchzip {
  name = "linux-x64-tools";
  url = "https://storage.googleapis.com/flutter_infra_release/flutter/b3af521a050e6ef076778bcaf16e27b2521df8f8/linux-x64/artifacts.zip";
  sha256 = "sha256-GvLhf08qiLwI6ky6TAMeXummzXPLmYkdBdOglX3uPLs=";
  stripRoot = false;
}
