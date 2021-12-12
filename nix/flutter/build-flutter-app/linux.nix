{
  callPackage,
  makeWrapper,
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
    linux-x64-linux-x64-flutter-gtk
    linux-x64-profile-linux-x64-flutter-gtk
    linux-x64-release-linux-x64-flutter-gtk
  ;

  linux = callPackage ./../platforms/linux.nix {};
in

stdenv.mkDerivation (args // {
  inherit src name version;

  nativeBuildInputs =
    [
      makeWrapper
    ]
    ++ nativeBuildInputs
    ++ linux.packages
  ;

  preBuild =
    preBuild
    + ''
      pushd "$HOME/.cache/flutter"

      ln -s ${linux-x64-linux-x64-flutter-gtk}/* artifacts/engine/linux-x64
      ln -s ${linux-x64-profile-linux-x64-flutter-gtk} artifacts/engine/linux-x64-profile
      ln -s ${linux-x64-release-linux-x64-flutter-gtk} artifacts/engine/linux-x64-release

      ${createCacheStamp { name = "linux-sdk"; from = "engine"; }}

      popd
    ''
    + linux.shellHook
  ;

  buildPhase = ''
    runHook preBuild

    flutter build linux --no-pub

    runHook postBuild
  '';

  installPhase = ''
    runHook preInstall

    mkdir -p $out/bin/
    mv build/linux/x64/release/bundle/ $out/bundle

    makeWrapper "$out/bundle/${name}" "$out/bin/${name}" \
      --set LD_LIBRARY_PATH "$LD_LIBRARY_PATH"

    runHook postInstall
  '';
})
