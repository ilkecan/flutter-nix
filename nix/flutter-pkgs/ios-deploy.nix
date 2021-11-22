{
  fetchzip ? (import <nixpkgs> { }).fetchzip,
  ...
}:

fetchzip {
  name = "ios-deploy";
  url = "https://storage.googleapis.com/flutter_infra_release/ios-usb-dependencies/ios-deploy/ee3aec1a303dd0b52bb7b798b5a25c20e2614733/ios-deploy.zip";
  sha256 = "sha256-1uUPy6Gj1tff7Ud9FrL0+9NH0dp2nL9PagGW2CogWqY=";
  stripRoot = false;
}
