{
  fetchzip ? (import <nixpkgs> {}).fetchzip,
  ...
}:

fetchzip {
  name = "libplist";
  url = "https://storage.googleapis.com/flutter_infra_release/ios-usb-dependencies/libplist/20a2f8dbddcf1a96ad4c720b9afd1d0876d17ffc/libplist.zip";
  sha256 = "sha256-wiSG9JKCZq2o30d43i203vpRim5Wb3dysQkL4HNrJo4=";
  stripRoot = false;
}
