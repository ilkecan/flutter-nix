{
  fetchzip ? (import <nixpkgs> {}).fetchzip,
  ...
}:

fetchzip {
  name = "flutter_patched_sdk";
  url = "https://storage.googleapis.com/flutter_infra_release/flutter/b3af521a050e6ef076778bcaf16e27b2521df8f8/flutter_patched_sdk.zip";
  sha256 = "sha256-5lBKB/hTQkuIWq2RDm2XqWRj3Rt/VBVcQjVC0H/CgWo=";

  passthru = {
    stamp = "b3af521a050e6ef076778bcaf16e27b2521df8f8";
  };
}
