{
  fetchzip ? (import <nixpkgs> {}).fetchzip,
  ...
}:

fetchzip {
  name = "sky_engine";
  url = "https://storage.googleapis.com/flutter_infra_release/flutter/b3af521a050e6ef076778bcaf16e27b2521df8f8/sky_engine.zip";
  sha256 = "sha256-oEoZp2DXwIKFTqXQuXSTTYveXyTD5hwVONkJNHBlMpc=";
}
