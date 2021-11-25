{ fetchzip }:

{
  name,
  version,
  sha256,
  url ? "https://pub.dartlang.org",
}:

fetchzip {
  name = "${name}-${version}";
  url = "${url}/packages/${name}/versions/${version}.tar.gz";
  inherit sha256;
  stripRoot = false;
}
