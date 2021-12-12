{
  callPackage,
  chromium,
}:

let
  inherit (callPackage ./lib.nix {})
    exportEnvVars
  ;
in
{
  packages = [
  ];

  shellHook =
    ''
      flutter config \
        --enable-web \
        > /dev/null
    ''
    + exportEnvVars {
      CHROME_EXECUTABLE = "${chromium}/bin/chromium";
    }
  ;
}
