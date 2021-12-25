{
  androidSdk,
  sdkPlatformApiLevels
}:

androidSdk (sdkPkgs:
  let
    getSdkPlatform = apiLevel: sdkPkgs."platforms-android-${toString apiLevel}";
  in
  with sdkPkgs; [
    build-tools-29-0-2
    cmdline-tools-latest
    platform-tools
  ] ++ map getSdkPlatform sdkPlatformApiLevels
)
