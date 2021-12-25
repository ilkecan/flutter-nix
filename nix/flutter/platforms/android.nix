{
  android-studio,
  callPackage,
  sdkPlatformApiLevels,
}:

let
  inherit (callPackage ./lib.nix {})
    exportEnvVars
  ;

  androidSdk =
    callPackage ./../../android-sdk.nix { inherit sdkPlatformApiLevels; };
in
{
  packages = [
    androidSdk
    android-studio
  ];

  shellHook =
    ''
      flutter config \
        --enable-android \
        --android-studio-dir ${android-studio} \
        > /dev/null
    ''
    + exportEnvVars {
      JAVA_HOME = "${android-studio.unwrapped}/jre";
    }
  ;
}
