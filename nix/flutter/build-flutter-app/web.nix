{
  callPackage,
  sdkDependencies,
  stdenv,
}:

{
  src,
  name,
  version,
  nativeBuildInputs,
  preBuild,
  ...
}@args:

let
  inherit (callPackage ./lib.nix {})
    createCacheStamp
  ;

  inherit (sdkDependencies)
    flutter-web-sdk-linux-x64
  ;

  web = callPackage ./../platforms/web.nix {};
in

stdenv.mkDerivation (args // {
  inherit src name version;

  nativeBuildInputs =
    [ ]
    ++ nativeBuildInputs
    ++ web.packages
  ;

  preBuild =
    preBuild
    + ''
      pushd "$HOME/.cache/flutter"

      ln -s ${flutter-web-sdk-linux-x64} flutter_web_sdk

      ${createCacheStamp { name = "flutter_web_sdk"; from = "engine"; }}

      popd
    ''
    + web.shellHook
  ;

  buildPhase = ''
    runHook preBuild

    flutter build web --no-pub

    runHook postBuild
  '';

  installPhase = ''
    runHook preInstall

    mv build/web $out

    runHook postInstall
  '';
})
