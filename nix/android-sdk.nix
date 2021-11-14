{
  androidenv,
  ...
}:


androidenv.composeAndroidPackages {
  platformVersions = [ "30" ];
  buildToolsVersions = [ "29.0.2" ];
  abiVersions = [ "x86_64"];
}
