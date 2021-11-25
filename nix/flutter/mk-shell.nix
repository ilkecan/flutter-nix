{
  callPackage,
  chromium,
  flutter,
  flutter-nix,
  mkShell,
}:

{
  packages ? [ ],
  shellHook ? "",
  ...
}@args:

let
  androidSdk = callPackage ./../android-sdk.nix { };

  linux = callPackage ./platforms/linux.nix { };
in
mkShell (args // {
  packages = [
    flutter
    flutter.dart

    # flutter-nix
    flutter-nix.translator

    # flutter-web
    chromium

    # flutter-android
    androidSdk
  ]
  ++ linux.packages
  ++ packages;

  shellHook = ''
    export CHROME_EXECUTABLE="${chromium}/bin/chromium"
  ''
  + linux.shellHook
  + shellHook;
})
