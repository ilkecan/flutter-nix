{
  fetchzip ? (import <nixpkgs> {}).fetchzip,
  ...
}:

fetchzip {
  name = "linux-x64-profile-flutter-gtk-tools";
  url = "https://storage.googleapis.com/flutter_infra_release/flutter/b3af521a050e6ef076778bcaf16e27b2521df8f8/linux-x64-profile/linux-x64-flutter-gtk.zip";
  sha256 = "sha256-7yG5QHzj98LHLTiL2dryxJTk9CYLW8vNfv6RkfJ97F8=";
  stripRoot = false;
}
