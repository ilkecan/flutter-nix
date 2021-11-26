{
  callPackage,
  chromium,
}:

let
  inherit (callPackage ./. {})
    exportEnvVars
  ;

  envVars = {
    CHROME_EXECUTABLE = "${chromium}/bin/chromium";
  };
in
{
  packages = [
    chromium
  ];

  shellHook = exportEnvVars envVars;
}
