{
  fetchzip ? (import <nixpkgs> { }).fetchzip,
  ...
}:

fetchzip {
  name = "flutter_patched_sdk_product";
  url = "https://storage.googleapis.com/flutter_infra_release/flutter/b3af521a050e6ef076778bcaf16e27b2521df8f8/flutter_patched_sdk_product.zip";
  sha256 = "sha256-+IqPY1/x0AdHJdVg6Vsil4TsFVnW2QfPOT2IniswxPY=";
}
