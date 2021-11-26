{
  callPackage,
}:

let
  androidSdk = callPackage ./../../android-sdk.nix { };
in
{
  packages = [
    androidSdk
  ];

  shellHook = ''
  '';
}
