{
  fetchzip ? (import <nixpkgs> { }).fetchzip,
  ...
}:

fetchzip {
  name = "linux-x64-release-flutter-gtk-tools";
  url = "https://storage.googleapis.com/flutter_infra_release/flutter/b3af521a050e6ef076778bcaf16e27b2521df8f8/linux-x64-release/linux-x64-flutter-gtk.zip";
  sha256 = "sha256-DKFk2B8gME/eUtAHLggLFodj/nqDK/M+rLVWtFCxhUM=";
  stripRoot = false;
}
