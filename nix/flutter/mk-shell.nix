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
    optionalString
    optionals
  ;

  linux = callPackage ./platforms/linux.nix { };
  android = callPackage ./platforms/android.nix { };
  web = callPackage ./platforms/web.nix { };
in
mkShell (args // {
  packages = [
    flutter
    flutter.dart
  ] ++ packages
    ++ optionals enableFlutterNix [ flutter-nix.translator ]
    ++ optionals enableLinux linux.packages
    ++ optionals enableAndroid android.packages
    ++ optionals enableWeb web.packages
  ;

  shellHook = shellHook
    + optionalString enableLinux linux.shellHook
    + optionalString enableAndroid android.shellHook
    + optionalString enableWeb web.shellHook
  ;
})
