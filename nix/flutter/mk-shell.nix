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

  enableAll ? false,
  android ? { },
  flutterNix ? { },
  linux ? { },
  web ? { },
  ...
}@args:

let
  inherit (builtins)
    attrNames
    removeAttrs
    all
  ;
  inherit (lib)
    assertMsg
    optionals
  ;
  inherit (lib.generators)
    toPretty
  ;

  default = {
    android = {
      enable = enableAll || false;
      sdkPlatformApiLevels = [ ];
    };

    linux = {
      enable = enableAll || false;
    };

    web = {
      enable = enableAll || false;
    };

    flutterNix = {
      enable = true;
    };
  };

  android' =
    default.android
    // android
    // callPackage ./platforms/android.nix {
      inherit (android') sdkPlatformApiLevels;
    }
  ;

  linux' =
    default.linux
    // linux
    // callPackage ./platforms/linux.nix { }
  ;

  web' =
    default.web
    // web
    // callPackage ./platforms/web.nix { }
  ;

  flutterNix' =
    default.flutterNix
    // flutterNix
  ;

  assertAttrNames = name:
    let
      expectedAttrNames = attrNames default.${name};
      unknownAttrs = removeAttrs (args.${name} or {}) expectedAttrNames;
    in
    assertMsg (unknownAttrs == {})
      "flutter-nix.mkShell.${name} has unexpected attribute(s): ${
      toPretty {} (attrNames unknownAttrs)}, attributes must be one of ${
      toPretty {} expectedAttrNames}"
    ;
in

assert all assertAttrNames (attrNames default);

mkShell (
  removeAttrs args [
    "android"
    "enableAll"
    "flutterNix"
    "linux"
    "web"
  ]
  // {
    packages =
      [
        flutter
        flutter.dart
      ]
      ++ packages
      ++ optionals linux'.enable linux'.packages
      ++ optionals android'.enable android'.packages
      ++ optionals web'.enable web'.packages
      ++ optionals flutterNix'.enable [
        flutter-nix.translator
      ]
    ;

    shellHook = shellHook
      + (if linux'.enable then linux'.shellHook else ''
          flutter config --no-enable-linux-desktop > /dev/null
        '')
      + (if android'.enable then android'.shellHook else ''
          flutter config --no-enable-android > /dev/null
        '')
      + (if web'.enable then web'.shellHook else ''
          flutter config --no-enable-web > /dev/null
        '')
    ;
  }
)
