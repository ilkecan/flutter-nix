{
  fetchzip ? (import <nixpkgs> { }).fetchzip,
  ...
}:

fetchzip {
  name = "linux-x64-flutter-gtk-tools";
  url = "https://storage.googleapis.com/flutter_infra_release/flutter/b3af521a050e6ef076778bcaf16e27b2521df8f8/linux-x64/linux-x64-flutter-gtk.zip";
  sha256 = "sha256-6WsO0Tu1gxPAmZDpr1Al8YjqlLGucwpPDNgbKGQwgI0=";
  stripRoot = false;

  passthru = {
    stamp = "b3af521a050e6ef076778bcaf16e27b2521df8f8";
  };
}
