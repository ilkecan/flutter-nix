{
  fetchzip ? (import <nixpkgs> {}).fetchzip,
  ...
}:

fetchzip {
  name = "material-fonts";
  url = "https://storage.googleapis.com/flutter_infra_release/flutter/fonts/bd151aa3c2f7231344411a01dba4ef61b3cd56b2/fonts.zip";
  sha256 = "sha256-lmUjv2KDZqLbvBQJrcd2SkWyvYvOrqc1p+KBVPm6YqQ=";
  stripRoot = false;

  passthru = {
    stamp = "flutter_infra_release/flutter/fonts/bd151aa3c2f7231344411a01dba4ef61b3cd56b2/fonts.zip";
  };
}
