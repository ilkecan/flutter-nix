{
  flutter,
  lib,
  writeText,
}:

let
  inherit (builtins)
    readDir
    toFile
    toJSON
  ;

  inherit (lib)
    fileContents
    filterAttrs
    genAttrs
    hasInfix
    hasSuffix
    mapAttrsToList
    optionalAttrs
    removeSuffix
  ;

  inherit (lib.strings)
    sanitizeDerivationName
  ;

  sdkDependencyNames =
    let
      files = filterAttrs
        (file: type: type == "regular" && hasSuffix ".version" file)
        (readDir "${flutter.unwrapped}/bin/internal");
    in
    mapAttrsToList (file: _: removeSuffix ".version" file) files;

  getVersion = name: fileContents "${flutter.unwrapped}/bin/internal/${name}.version";

  sdkDependencyVersions = genAttrs sdkDependencyNames getVersion;

  prefix = "https://storage.googleapis.com";

  engineDependency = name: {
    name = sanitizeDerivationName name;
    url = "${prefix}/flutter_infra_release/flutter/${sdkDependencyVersions.engine}/${name}.zip";
    stripRoot = !hasInfix "/" name;
  };

  iosUsbDependency = name: {
    name = sanitizeDerivationName name;
    url = "${prefix}/flutter_infra_release/ios-usb-dependencies/${name}/${sdkDependencyVersions.${name}}/${name}.zip";
    stripRoot = false;
  };

  gradleWrapper = {
    name = "gradle_wrapper";
    url = "${prefix}/${sdkDependencyVersions.gradle_wrapper}";
    stripRoot = false;
  };

  materialFonts = {
    name = "material_fonts";
    url = "${prefix}/${sdkDependencyVersions.material_fonts}";
    stripRoot = false;
  };

  sdkDependencies = {
    common = [
      (engineDependency "flutter_patched_sdk")
      (engineDependency "flutter_patched_sdk_product")
      (engineDependency "linux-x64/artifacts")
      (engineDependency "linux-x64/font-subset")
      (engineDependency "sky_engine")
      gradleWrapper
      materialFonts

      (iosUsbDependency "ios-deploy")
      (iosUsbDependency "libimobiledevice")
      (iosUsbDependency "libplist")
      (iosUsbDependency "openssl")
      (iosUsbDependency "usbmuxd")
    ];

    android = [
      (engineDependency "android-arm-profile/linux-x64")
      (engineDependency "android-arm-release/linux-x64")
      (engineDependency "android-arm64-profile/linux-x64")
      (engineDependency "android-arm64-release/linux-x64")
      (engineDependency "android-x64-profile/linux-x64")
      (engineDependency "android-x64-release/linux-x64")
    ];

    linux = [
      (engineDependency "linux-x64-profile/linux-x64-flutter-gtk")
      (engineDependency "linux-x64-release/linux-x64-flutter-gtk")
      (engineDependency "linux-x64/linux-x64-flutter-gtk")
    ];

    web = [
      (engineDependency "flutter-web-sdk-linux-x64")
    ];
  };
in
writeText "flutter-sdk-dependencies.json" (toJSON sdkDependencies)
