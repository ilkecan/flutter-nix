{
  androidSdk,
  ...
}:

androidSdk (sdkPkgs: with sdkPkgs; [
  build-tools-29-0-2
  cmdline-tools-latest
  platform-tools
  platforms-android-30
])
