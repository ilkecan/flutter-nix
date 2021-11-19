{
  fetchzip ? (import <nixpkgs> {}).fetchzip,
  ...
}:

fetchzip {
  name = "gradle-wrapper";
  url = "https://storage.googleapis.com/flutter_infra_release/gradle-wrapper/fd5c1f2c013565a3bea56ada6df9d2b8e96d56aa/gradle-wrapper.tgz";
  sha256 = "sha256-SRgOgd0iOFw8cD09GYjPOz9KJkxJg9ORD5ybELVSrk4=";

  passthru = {
    stamp = "flutter_infra_release/gradle-wrapper/fd5c1f2c013565a3bea56ada6df9d2b8e96d56aa/gradle-wrapper.tgz";
  };
}
