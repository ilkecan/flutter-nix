{
  lib,
}:

let
  inherit (lib)
    concatStrings
    mapAttrsToList
  ;

  exportEnvVar = key: value: ''
    export ${key}="${value}"
  '';

  exportEnvVars = envVars:
    concatStrings (mapAttrsToList exportEnvVar envVars);
in
{
  inherit
    exportEnvVars
  ;
}
