{
  fetchzip ? (import <nixpkgs> {}).fetchzip,
  ...
}:

fetchzip {
  name = "usbmuxd";
  url = "https://storage.googleapis.com/flutter_infra_release/ios-usb-dependencies/usbmuxd/c7d7d1a03f65a27be2eddb13d1f2b0c0e7a60ec6/usbmuxd.zip";
  sha256 = "sha256-KC/ZITgQn3YUnyBgI6t7rhWoEvSbHvzwiswS60efzFA=";
  stripRoot = false;
}
