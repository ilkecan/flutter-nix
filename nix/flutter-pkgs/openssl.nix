{
  fetchzip ? (import <nixpkgs> { }).fetchzip,
  ...
}:

fetchzip {
  name = "openssl";
  url = "https://storage.googleapis.com/flutter_infra_release/ios-usb-dependencies/openssl/e2e09d9fba1187f8d6aafaa34d4172f56f1ffb72/openssl.zip";
  sha256 = "sha256-nT4oYUb1hX1B1gz6/c/+1QKC1xgz+FUEvYZgYPAMNxM=";
  stripRoot = false;
}
