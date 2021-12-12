{
  callPackage,
  flutter,
  flutter-nix,
  lib,
  mkShell,
}:

{
  packages ? [ ],
  shellHook ? "",
  enableFlutterNix ? true,
  enableAll ? false,
  enableLinux ? enableAll || false,
  enableAndroid ? enableAll || false,
  enableWeb ? enableAll || false,
  ...
}@args:

let
  inherit (lib)
    optionals
  ;

  linux = callPackage ./platforms/linux.nix { };
  android = callPackage ./platforms/android.nix { };
  web = callPackage ./platforms/web.nix { };
in
mkShell (args // {
  packages =
    [
      flutter
      flutter.dart
    ]
    ++ packages
    ++ optionals enableLinux linux.packages
    ++ optionals enableAndroid android.packages
    ++ optionals enableWeb web.packages
    ++ optionals enableFlutterNix [
      flutter-nix.translator
    ]
  ;

  shellHook = shellHook
    + (if enableLinux then linux.shellHook else ''
        flutter config --no-enable-linux-desktop > /dev/null
      '')
    + (if enableAndroid then android.shellHook else ''
        flutter config --no-enable-android > /dev/null
      '')
    + (if enableWeb then web.shellHook else ''
        flutter config --no-enable-web > /dev/null
      '')
  ;
})
