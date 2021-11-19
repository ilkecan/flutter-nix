{
  fetchzip ? (import <nixpkgs> {}).fetchzip,
  ...
}:

fetchzip {
  name = "libimobiledevice";
  url = "https://storage.googleapis.com/flutter_infra_release/ios-usb-dependencies/libimobiledevice/2ba8188ed97d8b05670845e5b5954e2fe0f54784/libimobiledevice.zip";
  sha256 = "sha256-1DMAvLN0SOxFOEy4Zta+uGMPiYqoP4DPDiBVgmp9wVY=";
  stripRoot = false;
}
